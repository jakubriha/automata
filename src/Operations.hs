module Operations
  ( charsToLabels
  , run
  ) where

import Data.Set (member)

import Types.Fwa
import Helpers (findSingle)

charsToLabels :: String -> [Label]
charsToLabels = fmap (: [])

run :: Fwa -> [Label] -> Bool
run fwa =
  run' (startState fwa)
    where
      run' :: State -> [Label] -> Bool
      run' currentState [] = currentState `member` finalStates fwa
      run' currentState (x:xs) =
        let validTransition = findValidTransition fwa currentState x
        in
          case validTransition of
            Just transition -> run' (finalState transition) xs
            _ -> False

findValidTransition :: Fwa -> State -> Label -> Maybe Transition
findValidTransition fwa currentState currentLabel =
  findSingle condition (transitions fwa)
    where
      condition :: Transition -> Bool
      condition transition = state transition == currentState && label transition == currentLabel

