module Types.Fwa
  ( State
  , Label
  , Alphabet
  , Transition (..)
  , Fwa (..)
  , makeFwa
  ) where

import Data.Set (Set, isSubsetOf, member)

type State =
  String

type Label =
  String

type Alphabet =
  Set Label

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
    { states :: Set State
    , startState :: State
    , finalStates :: Set State
    , transitions :: Set Transition
    , alphabet :: Alphabet
    } deriving (Show)

makeFwa :: Set State -> State -> Set State -> Set Transition -> Alphabet -> Maybe Fwa
makeFwa states startState finalStates transitions alphabet =
  if (finalStates `isSubsetOf` states) && (startState `member` states)
     then Just(Fwa states startState finalStates transitions alphabet)
     else Nothing
