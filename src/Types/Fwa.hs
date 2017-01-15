module Types.Fwa
  ( Label
  , Transition (..)
  , Fwa (..)
  , states
  ) where

import Data.List (nub)
import Data.Set (Set, isSubsetOf, member, toList)

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
    { startState :: s
    , finalStates :: Set s
    , transitions :: [Transition s]
    } deriving (Show)

states :: (Eq s) => Fwa s -> [s]
states (Fwa startState finalStates transitions) =
  nub ([startState] ++ toList finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

