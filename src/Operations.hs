module Operations
  ( charsToLabels
  , run
  , Operations.union
  , Operations.intersect
  ) where

import Types.Fwa
import Data.List as List

charsToLabels :: String -> [Label]
charsToLabels = fmap (: [])

run :: Fwa -> [Label] -> Bool
run fwa =
  run' (startStates fwa)
    where
      run' currentStates [] =
        currentStates `List.intersect` finalStates fwa /= []
      run' currentStates (x:xs) =
        run' (post fwa currentStates x) xs

post :: Fwa -> [State] -> Label -> [State]
post fwa currentStates label =
  fmap finalState $ filter isApplicableTransition $ transitions fwa
    where
      isApplicableTransition (Transition tLabel state _) =
        tLabel == label && state `elem` currentStates

union :: Fwa -> Fwa -> Fwa
union (Fwa startStates1 finalStates1 transitions1) (Fwa startStates2 finalStates2 transitions2) =
  Fwa
    (startStates1 `List.union` startStates2)
    (finalStates1 `List.union`  finalStates2)
    (transitions1 `List.union` transitions2)

intersect :: Fwa -> Fwa -> Fwa
intersect fwa1@(Fwa startStates1 finalStates1 transitions1) (Fwa startStates2 finalStates2 transitions2) =
  let
    mergeStates first second = "(" ++ first ++ "," ++ second ++ ")"
    transitionsPerLabel label =
      [ Transition label (mergeStates state1 state2) (mergeStates final1 final2)
      | (Transition label1 state1 final1) <- transitions1
      , (Transition label2 state2 final2) <- transitions2
      , label1 == label && label2 == label
      ]
    transitions = concatMap transitionsPerLabel (labels fwa1)
  in
    Fwa
      [mergeStates state1 state2 | state1 <- startStates1, state2 <- startStates2]
      [mergeStates state1 state2 | state1 <- finalStates1, state2 <- finalStates2]
      transitions

