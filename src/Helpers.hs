module Helpers
  ( findSingle
  , findSingleInSet
  ) where

import Data.Set as Set

findSingle :: (a -> Bool) -> [a] -> Maybe a
findSingle predicate list =
  case Prelude.filter predicate list of
    [x] -> Just x
    _   -> Nothing

findSingleInSet :: (a -> Bool) -> Set a -> Maybe a
findSingleInSet predicate list =
  case Set.toList $ Set.filter predicate list of
    [x] -> Just x
    _   -> Nothing

