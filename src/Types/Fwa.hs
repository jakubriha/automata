module Types.Fwa
  ( Label
  , Alphabet
  , Transition (..)
  , Fwa (..)
  , makeFwa
  ) where

import Data.Set (Set, isSubsetOf, member)

type Label =
  String

type Alphabet =
  Set Label

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
    , alphabet :: Alphabet
    } deriving (Show)

makeFwa :: Ord s => Set s -> s -> Set s -> Set (Transition s) -> Alphabet -> Maybe (Fwa s)
makeFwa states startState finalStates transitions alphabet =
  if (finalStates `isSubsetOf` states) && (startState `member` states)
     then Just(Fwa states startState finalStates transitions alphabet)
     else Nothing

