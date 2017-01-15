module Helpers
  ( findSingle
  ) where

import Data.Set as Set

findSingle :: (a -> Bool) -> Set.Set a -> Maybe a
findSingle predicate set =
  case Set.toList $ Set.filter predicate set of
    [x] -> Just x
    _   -> Nothing

