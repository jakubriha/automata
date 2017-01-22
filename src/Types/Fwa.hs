module Types.Fwa
  ( Label
  , State
  , Transition (..)
  , Fwa (..)
  , states
  ) where

import Data.List (nub)

type Label =
  State

type State =
  String

data Transition =
  Transition
    { label :: Label
    , state :: State
    , finalState :: State
    } deriving (Eq, Ord)

instance Show Transition where
  show (Transition label state finalState) =
    show label ++ "(" ++ show state ++ ")->" ++ show finalState

data Fwa =
  Fwa
    { startStates :: [State]
    , finalStates :: [State]
    , transitions :: [Transition]
    } deriving (Show)

states :: Fwa -> [State]
states (Fwa startStates finalStates transitions) =
  nub (startStates ++ finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

