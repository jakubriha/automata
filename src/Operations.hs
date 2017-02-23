module Operations
  ( charsToSymbols
  , run
  , Operations.union
  , Operations.intersect
  , determinize
  , complement
  , isEmpty
  ) where

import Types.Fa
import Data.List as List

charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

containFinalState :: Eq sta => Fa sym sta -> [sta] -> Bool
containFinalState fa states =
  states `List.intersect` finalStates fa /= []

run :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> Bool
run fa =
  run' (initialStates fa)
    where
      run' currentStates [] =
        containFinalState fa currentStates
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs

post :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> sym -> [sta]
post fa currentStates symbol =
  fmap finalState $ filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

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
    finalStates = (nub . filter (containFinalState fa) . concatMap mapper) transitions
  in
    Fa [initialStates fa] finalStates transitions

complement :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
complement =
  updateFinalStates . determinize
    where
      updateFinalStates fa@(Fa initialStates finalStates transitions) =
        Fa initialStates (states fa \\ finalStates) transitions

isEmpty :: (Eq sym, Eq sta) => Fa sym sta -> Bool
isEmpty =
  (== []) . finalStates . determinize

