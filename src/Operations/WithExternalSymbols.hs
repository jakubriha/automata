{-# LANGUAGE MonadComprehensions, BangPatterns, ScopedTypeVariables #-}

{-|
Module      : Operations.WithExternalSymbols
Description : Contains FA operations. Each operation has one extra parameter to
              pass an external alphabet which is used instead of the implicit
              alphabet.
-}
module Operations.WithExternalSymbols
  ( isMacrostateAccepting
  , post
  , postForEachSymbol
  , complete
  , productUnion
  , intersect
  , determinize
  , complement
  ) where

import Types.Fa hiding (State, state, symbols)
import Helpers (andThen, guard)
import qualified Helpers
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State hiding (guard, return)
import Control.Monad.Loops (whileM_)

-- |Converts String to a list of symbols.
charsToSymbols :: String -> [Symbol]
charsToSymbols =
  fmap (: [])

-- |Determines whether a macro-state is accepting.
isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool
isMacrostateAccepting fa states =
  not $ null $ states `Set.intersection` finalStates fa

-- |Returns the post state of a state and a specific symbol.
post :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> sym -> Set sta
post fa currentStates symbol =
  Set.map target $ Set.filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol source _) =
        tSymbol == symbol && source `elem` currentStates

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set sym -> Set (Set sta)
postForEachSymbol fa state =
  Set.map (post fa state)

complete :: forall sym sta.( Ord sym, Ord sta) => Set sym -> Fa sym sta -> Fa sym (Set sta)
complete symbols fa@(Fa initialStates finalStates transitions) =
  Fa init final trans
    where
      init = Set.map Set.singleton initialStates
      final = Set.map Set.singleton finalStates

      wrap :: Ord a => Set a -> Set (Set a)
      wrap =
        Set.map Set.singleton

      wrapOrEmptySet :: Ord a => Set a -> Set (Set a)
      wrapOrEmptySet elements =
        if null elements
          then Set.singleton Set.empty
          else wrap elements

      trans :: Ord sym => Set (Transition sym (Set sta))
      trans =
        symbols `andThen` (\symbol ->
        Set.insert Set.empty (wrap $ states fa) `andThen` (\state ->
        wrapOrEmptySet (post fa state symbol) `andThen` (\postState ->
        Helpers.return $ Transition symbol state postState)))

-- |Creates a union of two FAs with product state ('sta1', 'sta2'). Note: Input FAs must be complete.
productUnion :: (Ord sym, Ord sta1, Ord sta2) => Set sym -> Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
productUnion symbols fa1@(Fa initialStates1 finalStates1 transitions1) fa2@(Fa initialStates2 finalStates2 transitions2) =
  let
    transitions =
      symbols `andThen` (\symbol ->
      transitions1 `andThen` (\(Transition symbol1 source1 target1) ->
      transitions2 `andThen` (\(Transition symbol2 source2 target2) ->
      guard (symbol1 == symbol && symbol2 == symbol) `andThen` (\_ ->
      Helpers.return $ Transition symbol (source1, source2) (target1, target2)))))
  in
    Fa
      (initialStates1 `andThen` (\initial1 -> initialStates2 `andThen` (\initial2 -> Helpers.return (initial1, initial2))))
      (Set.union
        (finalStates1 `andThen` (\final1 -> states fa2 `andThen` (\states2 -> Helpers.return (final1, states2))))
        (states fa1 `andThen` (\states1 -> finalStates2 `andThen` (\final2 -> Helpers.return (states1, final2)))))
      transitions

-- |Creates an intersection of two FAs.
intersect :: (Ord sym, Ord sta1, Ord sta2) => Set sym -> Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect symbols (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  let
    transitions =
      symbols `andThen` (\symbol ->
      transitions1 `andThen` (\(Transition symbol1 source1 target1) ->
      transitions2 `andThen` (\(Transition symbol2 source2 target2) ->
      guard (symbol1 == symbol && symbol2 == symbol) `andThen` (\_ ->
      Helpers.return $ Transition symbol (source1, source2) (target1, target2)))))
  in
    Fa
      (initialStates1 `andThen` (\initial1 -> initialStates2 `andThen` (\initial2 -> Helpers.return (initial1, initial2))))
      (finalStates1 `andThen` (\final1 -> finalStates2 `andThen` (\final2 -> Helpers.return (final1, final2))))
      transitions

type Front sta = Set (Set sta)
type NewStates sta = Set (Set sta)
type NewTransitions sym sta = Set (Transition sym (Set sta))
type InnerState sym sta = (Front sta, NewStates sta, NewTransitions sym sta)

frontNotEmpty :: State (InnerState sym sta) Bool
frontNotEmpty = state $ \oldState@(!front, _, _) ->
  (not $ null front, oldState)

moveRFromFrontToNewStates :: Ord sta => State (InnerState sym sta) (Set sta)
moveRFromFrontToNewStates = state $ \(!front, !newStates, !newTransitions) ->
  let
    r = Set.findMin front
    front' = Set.delete r front
    newStates' = Set.insert r newStates
  in
    (r, (front', newStates', newTransitions))

addStateAndTransitionsOfR :: (Ord sym, Ord sta) => Fa sym sta -> Set sym -> Set sta -> State (InnerState sym sta) ()
addStateAndTransitionsOfR fa symbols r = state $ \(!oldFront, !oldStates, !oldTransitions) ->
  let
    rWithSymbol = Set.map (\symbol -> (post fa r symbol, symbol)) symbols
    r' = Set.map fst rWithSymbol
    newTransitions = oldTransitions `Set.union` Set.map (\(newR, symbol) -> Transition symbol r newR) rWithSymbol
    newFront = oldFront `Set.union` Set.filter (`Set.notMember` oldStates) r'
  in
    ((), (newFront, oldStates, newTransitions))

whileBody :: (Ord sym, Ord sta) => Fa sym sta -> Set sym -> State (InnerState sym sta) ()
whileBody !fa !symbols =
  moveRFromFrontToNewStates >>= addStateAndTransitionsOfR fa symbols

while :: (Ord sym, Ord sta) => Fa sym sta -> Set sym -> State (InnerState sym sta) (Fa sym (Set sta))
while fa@(Fa initialStates finalStates _) !symbols = do
  whileM_ frontNotEmpty (whileBody fa symbols)
  (_, newStates, newTransitions) <- get
  Prelude.return $ Fa (Set.singleton initialStates) (newFinalStates newStates) newTransitions
    where
      newFinalStates = Set.filter $ not . Set.null . (`Set.intersection` finalStates)

determinize :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Fa sym (Set sta)
determinize !symbols !fa =
  evalState (while fa symbols) (Set.singleton (initialStates fa), Set.empty, Set.empty)

complement :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Fa sym (Set sta)
complement symbols =
  updateFinalStates . determinize symbols
    where
      updateFinalStates fa@(Fa initialStates finalStates transitions) =
        Fa initialStates (states fa Set.\\ finalStates) transitions
