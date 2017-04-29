module Types.Fta
  ( Symbol
  , State
  , Rank
  , RankedAlphabet
  , Transition (..)
  , Fta (..)
  , makeFta
  ) where

import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set

type Symbol =
  State

type State =
  String

type Rank =
  Int

type RankedAlphabet =
  Set (Symbol, Rank)

-- |Represents a transition of a FTA.
data Transition =
  Transition
    { symbol :: Symbol
    , inputStates :: Set State
    , finalState :: State
    } deriving (Eq, Ord)

instance Show Transition where
  show (Transition symbol inputStates finalState) =
    show symbol ++ "(" ++ show inputStates ++ ")->" ++ show finalState

-- |Represents a finite tree automaton (FTA).
data Fta =
  Fta
    { states :: Set State
    , finalStates :: Set State
    , transitions :: Set Transition
    , rankedAlphabet :: RankedAlphabet
    } deriving (Show)

makeFta :: Set State -> Set State -> Set Transition -> RankedAlphabet -> Maybe Fta
makeFta states finalStates transitions rankedAlphabet =
  if and [finalStates `Set.isSubsetOf` states]
     then Just (Fta states finalStates transitions rankedAlphabet)
     else Nothing

