module Types.Fta
  ( Label
  , Rank
  , RankedAlphabet
  , Transition (..)
  , Fta (..)
  , makeFta
  ) where

import Data.Set (Set, isSubsetOf)

type Label =
  String

type Rank =
  Int

type RankedAlphabet =
  Set (Label, Rank)

data Transition s =
  Transition
    { label :: Label
    , inputStates :: Set s
    , finalState :: s
    } deriving (Eq, Ord)

instance Show s => Show (Transition s) where
  show (Transition label inputStates finalState) =
    show label ++ "(" ++ show inputStates ++ ")->" ++ show finalState

data Fta s =
  Fta
    { states :: Set s
    , finalStates :: Set s
    , transitions :: Set (Transition s)
    , rankedAlphabet :: RankedAlphabet
    } deriving (Show)

makeFta :: Ord s => Set s -> Set s -> Set (Transition s) -> RankedAlphabet -> Maybe (Fta s)
makeFta states finalStates transitions rankedAlphabet =
  if and [finalStates `isSubsetOf` states]
     then Just (Fta states finalStates transitions rankedAlphabet)
     else Nothing

