module Operations
  ( charsToLabels
  , run
  ) where

import Types.Fwa
import Data.List (intersect)

charsToLabels :: String -> [Label]
charsToLabels = fmap (: [])

run :: Ord s => Fwa s -> [Label] -> Bool
run fwa =
  run' (startStates fwa)
    where
      run' currentStates [] =
        currentStates `intersect` finalStates fwa /= []
      run' currentStates (x:xs) =
        run' (post fwa currentStates x) xs

post :: (Eq s) => Fwa s -> [s] -> Label -> [s]
post fwa currentStates label =
  fmap finalState $ filter isApplicableTransition $ transitions fwa
    where
      isApplicableTransition (Transition tLabel state _) =
        tLabel == label && state `elem` currentStates

