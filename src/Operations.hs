module Operations
  ( charsToLabels
  , run
  ) where

import Types.Fwa
import Helpers (findSingle)

charsToLabels :: String -> [Label]
charsToLabels = fmap (: [])

run :: Ord s => Fwa s -> [Label] -> Bool
run fwa =
  run' (startState fwa)
    where
      run' currentState [] = currentState `elem` finalStates fwa
      run' currentState (x:xs) =
        let validTransition = findValidTransition fwa currentState x
        in
          case validTransition of
            Just transition -> run' (finalState transition) xs
            _ -> False

findValidTransition :: Eq s => Fwa s -> s -> Label -> Maybe (Transition s)
findValidTransition fwa currentState currentLabel =
  findSingle condition (transitions fwa)
    where
      condition transition = state transition == currentState && label transition == currentLabel

--union :: Fwa -> Fwa -> Fwa
--union fwa1 fwa2 =
--  let states = [ x ++ y | x <- states fwa1, y <- states fwa2 ]
--      finalStates = filter undefined states
--  in
--    Fwa
--      (Set.fromList states)
--      (startState fwa1 ++ startState fwa2)




