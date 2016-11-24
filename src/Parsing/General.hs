module Parsing.General
  ( loadAndParseFta
  , loadAndParseFwa
  ) where

import Data.ByteString as B

import Data.Set as Set

import Types.Fta as Fta
import Types.Fwa as Fwa
import Parsing.Fta (parseFta)
import Helpers (findSingle)

loadAndParseFta :: (Monad m) => FilePath -> IO (m Fta)
loadAndParseFta filePath =
  fmap parseFta (B.readFile filePath)

loadAndParseFwa :: (Monad m) => FilePath -> IO (m Fwa)
loadAndParseFwa filePath =
  fmap parseFwa (B.readFile filePath)

parseFwa :: (Monad m) => B.ByteString -> m Fwa
parseFwa fileContent =
  case parseFta fileContent of
    Left error -> fail error
    Right fta -> case ftaToFwa fta of
      Just fwa -> return fwa
      _ -> error "Cannot convert FTA to FWA"

ftaToFwa :: Fta -> Maybe Fwa
ftaToFwa (Fta states finalStates transitions rankedAlphabet) =
  case startStateFromTransitions transitions of
    Nothing -> Nothing
    Just startState ->
      let
        fwaTransitions = ((Set.map ftaToFwaTransition) . (Set.filter isStartTransition)) transitions
        alphabet = Set.map fst rankedAlphabet
      in
        Just (Fwa states startState finalStates fwaTransitions alphabet)

isStartTransition :: Fta.Transition -> Bool
isStartTransition = not . Set.null . inputStates

ftaToFwaTransition :: Fta.Transition -> Fwa.Transition
ftaToFwaTransition (Fta.Transition label inputStates finalState) =
  Fwa.Transition label (elemAt 0 inputStates) finalState

startStateFromTransitions :: Set Fta.Transition -> Maybe Fwa.State
startStateFromTransitions transitions =
  case findSingle (\transition -> Set.null (inputStates transition)) transitions of
    Just x -> Just (Fta.finalState x)
    _ -> Nothing


