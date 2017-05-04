{-# LANGUAGE MonadComprehensions #-}

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
  , intersect
  , determinize
  , complement
  , isEmpty
  , isSubsetOf
  , isUniversal
  ) where

import Types.Fa hiding (State, state, symbols)
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Control.Monad.State
import Control.Monad.Loops (whileM_)

-- |Converts String to a list of symbols.
charsToSymbols :: String -> [Symbol]
charsToSymbols =
  fmap (: [])

-- |Determines whether a macro-state is accepting.
isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool
isMacrostateAccepting fa states =
  states `Set.intersection` finalStates fa /= Set.empty

-- |Returns the post state of a state and a specific symbol.
post :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> sym -> Set sta
post fa currentStates symbol =
  fmap target $ Set.filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol source _) =
        tSymbol == symbol && source `elem` currentStates

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set sym -> Set (Set sta)
postForEachSymbol fa state =
  fmap (post fa state)

-- |Creates an intersection of two FAs.
intersect :: (Ord sym, Ord sta1, Ord sta2) => Set sym -> Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect symbols (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  let
    transitionsPerSymbol symbol =
        [ Transition symbol (source1, source2) (target1, target2)
        | (Transition symbol1 source1 target1) <- transitions1
        , (Transition symbol2 source2 target2) <- transitions2
        , symbol1 == symbol && symbol2 == symbol
        ]
    transitions = (Set.unions . Set.toList . fmap transitionsPerSymbol) symbols
  in
    Fa
      [(initial1, initial2) | initial1 <- initialStates1, initial2 <- initialStates2]
      [(final1, final2) | final1 <- finalStates1, final2 <- finalStates2]
      transitions

type Front sta = Set (Set sta)
type NewStates sta = Set (Set sta)
type NewTransitions sym sta = Set (Transition sym (Set sta))
type InnerState sym sta = (Front sta, NewStates sta, NewTransitions sym sta)

frontNotEmpty :: State (InnerState sym sta) Bool
frontNotEmpty = state $ \oldState@(front, _, _) ->
  (not $ null front, oldState)

moveRFromFrontToNewStates :: Ord sta => State (InnerState sym sta) (Set sta)
moveRFromFrontToNewStates = state $ \(front, newStates, newTransitions) ->
  let
    r = Set.findMin front
    front' = Set.delete r front
    newStates' = Set.insert r newStates
  in
    (r, (front', newStates', newTransitions))

addStateAndTransitionsOfR :: (Ord sym, Ord sta) => Fa sym sta -> Set sym -> Set sta -> State (InnerState sym sta) ()
addStateAndTransitionsOfR fa symbols r = state $ \(oldFront, oldStates, oldTransitions) ->
  let
    rWithSymbol = fmap (\symbol -> (post fa r symbol, symbol)) symbols
    r' = fmap fst rWithSymbol
    newTransitions = oldTransitions `Set.union` [Transition symbol r newR | (newR, symbol) <- rWithSymbol]
    newFront = oldFront `Set.union` [newR | newR <- r', newR `Set.notMember` oldStates]
  in
    ((), (newFront, oldStates, newTransitions))

whileBody :: (Ord sym, Ord sta) => Fa sym sta -> Set sym -> State (InnerState sym sta) ()
whileBody fa symbols =
  moveRFromFrontToNewStates >>= addStateAndTransitionsOfR fa symbols

while :: (Ord sym, Ord sta) => Fa sym sta -> Set sym -> State (InnerState sym sta) (Fa sym (Set sta))
while fa@(Fa initialStates finalStates _) symbols = do
  whileM_ frontNotEmpty (whileBody fa symbols)
  (_, newStates, newTransitions) <- get
  return $ Fa (Set.singleton initialStates) (newFinalStates newStates) newTransitions
    where
      newFinalStates = Set.filter $ not . Set.null . (`Set.intersection` finalStates)

-- |Converts a FA to an equivalent deterministic FA.
determinize :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Fa sym (Set sta)
determinize symbols fa =
  evalState (while fa symbols) (Set.singleton (initialStates fa), Set.empty, Set.empty)

-- |Creates a complement of a FA.
complement :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Fa sym (Set sta)
complement symbols =
  updateFinalStates . determinize symbols
    where
      updateFinalStates fa@(Fa initialStates finalStates transitions) =
        Fa initialStates (states fa Set.\\ finalStates) transitions

-- |Checks whether a FA accepts an empty language.
isEmpty :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Bool
isEmpty symbols =
  not . hasTerminatingPath symbols

hasTerminatingPath :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Bool
hasTerminatingPath symbols fa =
  not (null $ finalStates fa) && hasTerminatingPath' Set.empty (initialStates fa)
    where
      hasTerminatingPath' processed next
        | null next = False
        | not $ null $ next `Set.intersection` finalStates fa = True
        | otherwise =
          let
            processed' = processed `Set.union` next
            next' = newStates fa next symbols Set.\\ processed'
          in
            hasTerminatingPath' processed' next'

newStates :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set sym -> Set sta
newStates fa states symbols =
  Set.unions $ Set.toList $ postForEachSymbol fa states symbols 

-- |Checks whether the first FA is subset of the second FA using the classical algorithm.
isSubsetOf :: (Ord sym, Ord sta) => Set sym -> Set sym -> Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1Symbols fa2Symbols fa1 fa2 =
  isEmpty (fa1Symbols `Set.union` fa2Symbols) (intersect (fa1Symbols `Set.union` fa2Symbols) fa1 (complement fa2Symbols fa2))

-- |Checks whether a FA accepts all possible strings using the classical algorithm.
isUniversal :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Bool
isUniversal symbols fa =
  not (null $ states fa) && isSubsetOf symbols symbols universalFA fa
    where
      state = Set.findMin $ states fa
      transitions = fmap (\symbol -> Transition symbol state state) symbols
      universalFA = Fa (Set.singleton state) (Set.singleton state) transitions
