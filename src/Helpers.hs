module Helpers
  ( isSubsetOf
  , none
  , remove
  ) where

import Data.List (intersect)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf first second =
  first `intersect` second == first

none :: Foldable t => (a -> Bool) -> t a -> Bool
none predicate =
  not . any predicate

remove :: (a -> Bool) -> [a] -> [a]
remove predicate =
  filter (not. predicate)
