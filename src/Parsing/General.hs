module Parsing.General
  ( loadFta
  , loadFwa
  ) where

import Data.ByteString as B

import Data.Set as Set

import Types.Fta as Fta
import Types.Fwa as Fwa
import Parsing.Fta (parseFta)
import Helpers (findSingle, findSingleInSet)

loadFta :: (Monad m) => FilePath -> IO (m Fta)
loadFta filePath =
  fmap parseFta (B.readFile filePath)

loadFwa :: (Monad m) => FilePath -> IO (m (Fwa Fwa.Label Fwa.State))
loadFwa filePath =
  fmap parseFwa (B.readFile filePath)

parseFwa :: (Monad m) => B.ByteString -> m (Fwa Fwa.Label Fwa.State)
parseFwa fileContent =
  case parseFta fileContent of
    Left error -> fail error
    Right fta -> case ftaToFwa fta of
      Just fwa -> return fwa
      _ -> error "Cannot convert FTA to FWA"

ftaToFwa :: Fta -> Maybe (Fwa Fwa.Label Fwa.State)
ftaToFwa (Fta states finalStates transitions rankedAlphabet) =
  fmap mapper (findStartStateIn transitions)
    where
      fwaTransitions = (Set.map ftaToFwaTransition . Set.filter isNotStartTransition) transitions
      mapper startState = Fwa [startState] (Set.toList finalStates) (Set.toList fwaTransitions)

isNotStartTransition :: Fta.Transition -> Bool
isNotStartTransition = not . Set.null . inputStates

ftaToFwaTransition :: Fta.Transition -> Fwa.Transition Fwa.Label Fwa.State
ftaToFwaTransition (Fta.Transition label inputStates finalState) =
  Fwa.Transition label (elemAt 0 inputStates) finalState

findStartStateIn :: Set Fta.Transition -> Maybe Fta.State
findStartStateIn transitions =
  case findSingleInSet (Set.null . inputStates) transitions of
    Just x -> Just (Fta.finalState x)
    _ -> Nothing

