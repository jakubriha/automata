module Operations.Regular
  ( union
  ) where

import Types.Fa
import qualified Data.Set as Set
import Data.Set (Set, empty, toList, fromList, unions, intersection)
import Data.List ((\\), nub)

union :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `Set.union` initialStates2)
    (finalStates1 `Set.union` finalStates2)
    (transitions1 `Set.union` transitions2)
