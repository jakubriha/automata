module Operations
  ( charsToLabels
  , run
  , Operations.union
  , Operations.intersect
  ) where

import Types.Fa
import Data.List as List

charsToLabels :: String -> [Label]
charsToLabels = fmap (: [])

run :: (Eq i, Eq s) => Fa i s -> [i] -> Bool
run fa =
  run' (startStates fa)
    where
      run' currentStates [] =
        currentStates `List.intersect` finalStates fa /= []
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs

post :: (Eq i, Eq s) => Fa i s -> [s] -> i -> [s]
post fa currentStates label =
  fmap finalState $ filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tLabel state _) =
        tLabel == label && state `elem` currentStates

union :: (Eq i, Eq s) => Fa i s -> Fa i s -> Fa i s
union (Fa startStates1 finalStates1 transitions1) (Fa startStates2 finalStates2 transitions2) =
  Fa
    (startStates1 `List.union` startStates2)
    (finalStates1 `List.union`  finalStates2)
    (transitions1 `List.union` transitions2)

intersect :: Eq i => Fa i s -> Fa i s -> Fa i (s, s)
intersect fa1@(Fa startStates1 finalStates1 transitions1) (Fa startStates2 finalStates2 transitions2) =
  let
    transitionsPerLabel label =
      [ Transition label (state1, state2) (final1, final2)
      | (Transition label1 state1 final1) <- transitions1
      , (Transition label2 state2 final2) <- transitions2
      , label1 == label && label2 == label
      ]
    transitions = concatMap transitionsPerLabel (labels fa1)
  in
    Fa
      [(state1, state2) | state1 <- startStates1, state2 <- startStates2]
      [(state1, state2) | state1 <- finalStates1, state2 <- finalStates2]
      transitions

