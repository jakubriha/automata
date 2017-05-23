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
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set, empty, toList, fromList, unions, intersection)
import Data.List ((\\), nub)
import Control.Monad.State
import Control.Monad.Loops (whileM_)

-- |Converts String to a list of symbols.
charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

-- |Determines whether a macro-state is accepting.
isMacrostateAccepting :: Eq sta => Fa sym sta -> [sta] -> Bool
isMacrostateAccepting fa states =
  states `List.intersect` finalStates fa /= []

-- |Returns the post state of a state and a specific symbol.
post :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> sym -> [sta]
post fa currentStates symbol =
  fmap finalState $ filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Eq sym, Eq sta) => [sym] -> Fa sym sta -> [sta] -> [[sta]]
postForEachSymbol symbols fa state =
  fmap (post fa state) symbols

-- |Creates an intersection of two FAs.
intersect :: Eq sym => [sym] -> Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect symbols (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  let
    transitions =
      [ Transition symbol (source1, source2) (target1, target2)
      | symbol <- symbols
      , (Transition symbol1 source1 target1) <- transitions1
      , (Transition symbol2 source2 target2) <- transitions2
      , symbol1 == symbol && symbol2 == symbol
      ]
  in
    Fa
      [ (initial1, initial2) | initial1 <- initialStates1, initial2 <- initialStates2 ]
      [ (final1, final2) | final1 <- finalStates1, final2 <- finalStates2 ]
      transitions

type Front sta = [[sta]]
type NewStates sta = [[sta]]
type NewTransitions sym sta = [Transition sym [sta]]
type InnerState sym sta = (Front sta, NewStates sta, NewTransitions sym sta)

frontNotEmpty :: State (InnerState sym sta) Bool
frontNotEmpty = state $ \oldState@(front, _, _) ->
  (not $ null front, oldState)

moveRFromFrontToNewStates :: State (InnerState sym sta) [sta]
moveRFromFrontToNewStates = state $ \(r : front', newStates, newTransitions) ->
  (r, (front', r : newStates, newTransitions))

addStateAndTransitionsOfR :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> [sta] -> State (InnerState sym sta) ()
addStateAndTransitionsOfR fa symbols r = state $ \(front, oldStates, oldTransitions) ->
  let
    r' = fmap (post fa r) symbols
    newTransitions = fmap (\(symbol, newR) -> Transition symbol r newR) (zip symbols r')
    newFront = nub $ concatMap (\r -> if r `notElem` oldStates then [r] `List.union` front else front) r'
  in
    ((), (newFront, oldStates, newTransitions ++ oldTransitions))

whileBody :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> State (InnerState sym sta) ()
whileBody fa symbols =
  moveRFromFrontToNewStates >>= addStateAndTransitionsOfR fa symbols

while :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> State (InnerState sym sta) (Fa sym [sta])
while fa symbols = do
  whileM_ frontNotEmpty (whileBody fa symbols)
  (_, newStates, newTransitions) <- get
  return $ Fa [initialStates fa] (newFinalStates newStates) newTransitions
    where
      newFinalStates = filter (not . null . (`List.intersect` finalStates fa))

-- |Converts a FA to an equivalent deterministic FA.
determinize :: (Eq sym, Eq sta) => [sym] -> Fa sym sta -> Fa sym [sta]
determinize symbols fa =
  evalState (while fa symbols) ([initialStates fa], [], []) 

-- |Creates a complement of a FA.
complement :: (Eq sym, Eq sta) => [sym] -> Fa sym sta -> Fa sym [sta]
complement symbols =
  updateFinalStates . determinize symbols
    where
      updateFinalStates fa@(Fa initialStates finalStates transitions) =
        Fa initialStates (states fa \\ finalStates) transitions

-- |Checks whether a FA accepts an empty language.
isEmpty :: (Eq sym, Ord sta) => [sym] -> Fa sym sta -> Bool
isEmpty symbols =
  not . hasTerminatingPath symbols

hasTerminatingPath :: (Eq sym, Ord sta) => [sym] -> Fa sym sta -> Bool
hasTerminatingPath symbols fa =
  not (null $ finalStates fa) && hasTerminatingPath' empty (fromList $ initialStates fa)
    where
      hasTerminatingPath' processed next
        | null next = False
        | not $ null $ next `intersection` fromList (finalStates fa) = True
        | otherwise =
          let
            processed' = processed `Set.union` next
            next' = newStates symbols fa next Set.\\ processed'
          in
            hasTerminatingPath' processed' next'

newStates :: (Eq sym, Ord sta) => [sym] -> Fa sym sta -> Set sta -> Set sta
newStates symbols fa =
  fromList . nub . concatMap (concat . postForEachSymbol symbols fa . (: []))

-- |Checks whether the first FA is subset of the second FA using the naive algorithm.
isSubsetOf :: (Eq sym, Ord sta) => [sym] -> [sym] -> Fa sym sta -> Fa sym sta -> Bool
isSubsetOf symbols1 symbols2 fa1 fa2 =
  isEmpty combinedSymbols (intersect combinedSymbols fa1 (complement symbols2 fa2))
    where
      combinedSymbols =
        symbols1 `List.union` symbols2

-- |Checks whether a FA accepts all possible strings using the naive algorithm.
isUniversal :: (Eq sym, Ord sta) => [sym] -> Fa sym sta -> Bool
isUniversal symbols fa =
  not (null $ states fa) && isSubsetOf symbols symbols universalFA fa
    where
      state = head $ states fa
      transitions = fmap (\symbol -> Transition symbol state state) symbols
      universalFA = Fa [state] [state] transitions
