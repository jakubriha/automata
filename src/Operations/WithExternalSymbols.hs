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
  , transitionsCreator
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
  fmap finalState $ Set.filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set sym -> Set (Set sta)
postForEachSymbol fa state =
  fmap (post fa state)

transitionsCreator
  :: (Ord sym1, Ord sym2)
  => Set sym1
  -> Set sym2
  -> (sym1 -> sym2 -> sym)
  -> (sym1 -> sym2 -> Bool)
  -> Fa sym1 sta1
  -> Fa sym2 sta2
  -> Set (Transition sym (sta1, sta2))
transitionsCreator symbols1 symbols2 function predicate fa1 fa2 =
  [ Transition (function symbol1 symbol2) (state1, state2) (final1, final2)
  | symbol1 <- symbols1
  , symbol2 <- symbols2
  , predicate symbol1 symbol2
  , (Transition symbol1k state1 final1) <- transitions fa1
  , (Transition symbol2k state2 final2) <- transitions fa2
  , symbol1k == symbol1 && symbol2k == symbol2
  ]

-- |Creates an intersection of two FAs.
intersect :: Ord sym => Set sym -> Set sym -> Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect fa1Symbols fa2Symbols fa1 fa2 =
  Fa
    [(state1, state2) | state1 <- initialStates fa1, state2 <- initialStates fa2]
    [(state1, state2) | state1 <- finalStates fa1, state2 <- finalStates fa2]
    (transitionsCreator fa1Symbols fa2Symbols const (==) fa1 fa2)

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
  isEmpty (fa1Symbols `Set.union` fa2Symbols) (intersect fa1Symbols fa2Symbols fa1 (complement fa2Symbols fa2))

-- |Checks whether a FA accepts all possible strings using the classical algorithm.
isUniversal :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Bool
isUniversal symbols fa =
  not (null $ states fa) && isSubsetOf symbols symbols universalFA fa
    where
      state = Set.findMin $ states fa
      transitions = fmap (\symbol -> Transition symbol state state) symbols
      universalFA = Fa (Set.singleton state) (Set.singleton state) transitions
