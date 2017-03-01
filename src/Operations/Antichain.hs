module Operations.Antichain
  ( isUniversal
  ) where

import Types.Fa
import Operations.Regular (isMacrostateAccepting, post)
import Data.List (union, intersect, foldl', (\\))

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf first second =
  first `intersect` second == first

isLowerOrEqual :: Eq sta => [sta] -> [sta] -> Bool
isLowerOrEqual =
  isSubsetOf

data ForeachReturn sta = ReturnFalse | ReturnState [[sta]] [[sta]]

reducer :: Eq sta => ([sta] -> Bool) -> ForeachReturn sta -> [sta] -> ForeachReturn sta
reducer _ ReturnFalse _ = ReturnFalse
reducer acceptingMacrostate (ReturnState processed next) p
  | not $ acceptingMacrostate p = ReturnFalse
  | not $ or [s `isLowerOrEqual` p | s <- processed `union` next] = remove p
  | otherwise = ReturnState processed next
  where
    remove p =
      let
        processed' = processed \\ [s | s <- processed, p `isLowerOrEqual` s ]
        next' = p : (next \\ [s | s <- next, p `isLowerOrEqual` s ])
      in
        ReturnState processed' next'

foreach :: Eq sta => ([sta] -> Bool) -> ([sta] -> [[sta]]) -> [[sta]] -> [[sta]] -> [sta] -> ForeachReturn sta
foreach acceptingMacrostate po processed next r =
  foldl' (reducer acceptingMacrostate) (ReturnState processed next) (po r)

isUniversal' :: Eq sta => ([sta] -> Bool) -> ([sta] -> [[sta]]) -> [[sta]] -> [[sta]] -> Bool
isUniversal' acceptingMacrostate post processed next =
  case next of
    [] -> True
    _ ->
      let
        next' = tail next
        r = head next
        processed' = r : processed
        result = foreach acceptingMacrostate post processed' next' r
      in
        case result of
          ReturnState processed'' next'' -> isUniversal' acceptingMacrostate post processed'' next''
          _ -> False

post' :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> [[sta]]
post' fa state = fmap (($ state) . flip (post fa)) (symbols fa)

isUniversal :: (Eq sym, Eq sta) => Fa sym sta -> Bool
isUniversal fa =
  isMacrostateAccepting fa (initialStates fa) && isUniversal' (isMacrostateAccepting fa) (post' fa) [] [initialStates fa]
