module Types.Fta
    ( State
    , Label
    , Rank
    , RankedAlphabet
    , Transition (..)
    , Fta (..)
    , makeFta
    ) where

import Data.Set (Set, isSubsetOf)

type State = String

type Label = String

type Rank = Int

type RankedAlphabet = Set (Label, Rank)

data Transition = Transition { label :: Label
                             , inputStates :: Set State
                             , finalState :: State
                             } deriving (Eq, Ord)

instance Show Transition where
    show (Transition label inputStates finalState) = show label ++ "(" ++ show inputStates ++ ")->" ++ show finalState

data Fta = Fta { states :: Set State
               , finalStates :: Set State
               , transitions :: Set Transition
               , rankedAlphabet :: RankedAlphabet
               } deriving (Show)

makeFta :: Set State -> Set State -> Set Transition -> RankedAlphabet -> Maybe Fta
makeFta states finalStates transitions rankedAlphabet =
    if and [finalStates `isSubsetOf` states]
       then Just (Fta states finalStates transitions rankedAlphabet)
       else Nothing
