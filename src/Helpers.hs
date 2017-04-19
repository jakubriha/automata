module Helpers
  ( isSubsetOf
  , none
  , remove
  ) where

import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf first second =
  first `Set.intersection` second == first

none :: Foldable t => (a -> Bool) -> t a -> Bool
none predicate =
  not . any predicate

remove :: Ord a => (a -> Bool) -> Set a -> Set a
remove predicate =
  Set.filter (not. predicate)
