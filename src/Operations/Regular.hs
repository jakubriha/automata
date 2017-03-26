module Operations.Regular
  ( charsToSymbols
  , isMacrostateAccepting
  , run
  , post
  , postForEachSymbol
  , union
  , intersect
  , determinize
  , complement
  , isEmpty
  , isSubsetOf
  , isUniversal
  ) where

import Types.Fa
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set, empty, toList, fromList, unions, intersection)
import Data.List ((\\), nub)

charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

isMacrostateAccepting :: Eq sta => Fa sym sta -> [sta] -> Bool
isMacrostateAccepting fa states =
  states `List.intersect` finalStates fa /= []

run :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> Bool
run fa =
  run' (initialStates fa)
    where
      run' currentStates [] =
        isMacrostateAccepting fa currentStates
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs

post :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> sym -> [sta]
post fa currentStates symbol =
  fmap finalState $ filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

postForEachSymbol :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> [[sta]]
postForEachSymbol fa state =
  fmap (post fa state) (symbols fa)

union :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `List.union` initialStates2)
    (finalStates1 `List.union` finalStates2)
    (transitions1 `List.union` transitions2)

intersect :: Eq sym => Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect fa1@(Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  let
    transitionsPerSymbol symbol =
      [ Transition symbol (state1, state2) (final1, final2)
      | (Transition symbol1 state1 final1) <- transitions1
      , (Transition symbol2 state2 final2) <- transitions2
      , symbol1 == symbol && symbol2 == symbol
      ]
    transitions = concatMap transitionsPerSymbol (symbols fa1)
  in
    Fa
      [(state1, state2) | state1 <- initialStates1, state2 <- initialStates2]
      [(state1, state2) | state1 <- finalStates1, state2 <- finalStates2]
      transitions

determinize' :: (Eq sym, Eq sta) => [sta] -> [Transition sym [sta]] -> Fa sym sta -> [Transition sym [sta]]
determinize' current transitions fa =
  let
    postState = post fa current
    transition symbol = Transition symbol current (postState symbol)
    toReturn symbol =
      if transition symbol `elem` transitions
        then []
        else transition symbol : determinize' (postState symbol) (transition symbol : transitions) fa
  in
    nub $ concatMap toReturn (symbols fa)

determinize :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
determinize fa =
  let
    transitions = determinize' (initialStates fa) [] fa
    mapper (Transition _ state final) = [state, final]
    finalStatesFromInitialStates = [initialStates fa | isMacrostateAccepting fa (initialStates fa)]
    finalStates = (nub . filter (isMacrostateAccepting fa) . concatMap mapper) transitions
  in
    Fa [initialStates fa] (finalStatesFromInitialStates ++ finalStates) transitions

complement :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
complement =
  updateFinalStates . determinize
    where
      updateFinalStates fa@(Fa initialStates finalStates transitions) =
        Fa initialStates (states fa \\ finalStates) transitions

isEmpty :: (Eq sym, Ord sta) => Fa sym sta -> Bool
isEmpty =
  not . hasTerminatingPath

hasTerminatingPath :: (Eq sym, Ord sta) => Fa sym sta -> Bool
hasTerminatingPath fa =
  not (null $ finalStates fa) && hasTerminatingPath' empty (fromList $ initialStates fa)
    where
      hasTerminatingPath' processed next
        | null next = False
        | not $ null $ next `intersection` fromList (finalStates fa) = True
        | otherwise = 
          let
            processed' = processed `Set.union` next
            next' = newStates fa next Set.\\ processed'
          in
            hasTerminatingPath' processed' next'

newStates :: (Eq sym, Ord sta) => Fa sym sta -> Set sta -> Set sta
newStates fa =
  fromList . nub . concatMap (concat . postForEachSymbol fa . (: []))

isSubsetOf :: (Eq sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1 fa2 =
  isEmpty $ fa1 `intersect` complement fa2

isUniversal :: (Eq sym, Ord sta) => Fa sym sta -> Bool
isUniversal fa =
  not (null $ states fa) && (universalFA `isSubsetOf` fa)
    where
      state = head $ states fa
      transitions = fmap (\symbol -> Transition symbol state state) (symbols fa)
      universalFA = Fa [state] [state] transitions
