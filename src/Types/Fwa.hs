module Types.Fwa
  ( Label
  , Transition (..)
  , Fwa (..)
  ) where

import Data.Set (Set, isSubsetOf, member)

type Label =
  String

data Transition s =
  Transition
    { label :: Label
    , state :: s
    , finalState :: s
    } deriving (Eq, Ord)

instance Show s => Show (Transition s) where
  show (Transition label state finalState) =
    show label ++ "(" ++ show state ++ ")->" ++ show finalState

data Fwa s =
  Fwa
    { states :: Set s
    , startState :: s
    , finalStates :: Set s
    , transitions :: Set (Transition s)
    } deriving (Show)

