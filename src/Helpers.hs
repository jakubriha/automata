module Helpers
  ( isSubsetOf
  , none
  , remove
  , andThen
  , return
  , guard
  ) where

import Prelude hiding (return)
import Data.Set (Set)
import qualified Data.Set as Set

-- |Checks whether a set is a subset of the other set.
isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf first second =
  first `Set.intersection` second == first

-- |Determines whether no element of the structure satisfies the predicate. 
none :: Foldable t => (a -> Bool) -> t a -> Bool
none predicate =
  not . any predicate

-- |Filter all elements that do not satisfy the predicate.
remove :: Ord a => (a -> Bool) -> Set a -> Set a
remove predicate =
  Set.filter (not. predicate)

unions :: Ord a => Set (Set a) -> Set a
unions =
  Set.foldl Set.union Set.empty

return :: a -> Set a
return =
  Set.singleton

andThen :: (Ord a, Ord b) => Set a -> (a -> Set b) -> Set b
andThen monad f =
  (unions . Set.map f) monad

guard :: Bool -> Set ()
guard True = Set.singleton ()
guard False = Set.empty
