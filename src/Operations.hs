module Operations
  ( charsToLabels
  , run
  , Operations.union
  ) where

import Types.Fwa
import Data.List (intersect, union)

charsToLabels :: String -> [Label]
charsToLabels = fmap (: [])

run :: Fwa -> [Label] -> Bool
run fwa =
  run' (startStates fwa)
    where
      run' currentStates [] =
        currentStates `intersect` finalStates fwa /= []
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
    (startStates1 `Data.List.union` startStates2)
    (finalStates1 `Data.List.union`  finalStates2)
    (transitions1 `Data.List.union` transitions2)

