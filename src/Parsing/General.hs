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

loadAndParseFta :: (Monad m) => FilePath -> IO (m (Fta String))
loadAndParseFta filePath =
  fmap parseFta (B.readFile filePath)

loadAndParseFwa :: (Monad m) => FilePath -> IO (m (Fwa String))
loadAndParseFwa filePath =
  fmap parseFwa (B.readFile filePath)

parseFwa :: (Monad m) => B.ByteString -> m (Fwa String)
parseFwa fileContent =
  case parseFta fileContent of
    Left error -> fail error
    Right fta -> case ftaToFwa fta of
      Just fwa -> return fwa
      _ -> error "Cannot convert FTA to FWA"

ftaToFwa :: Ord s => Fta s -> Maybe (Fwa s)
ftaToFwa (Fta states finalStates transitions rankedAlphabet) =
  case startStateFromTransitions transitions of
    Nothing -> Nothing
    Just startState ->
      let
        fwaTransitions = (Set.map ftaToFwaTransition . Set.filter isStartTransition) transitions
        alphabet = Set.map fst rankedAlphabet
      in
        Just (Fwa states startState finalStates fwaTransitions alphabet)

isStartTransition :: Fta.Transition s -> Bool
isStartTransition = not . Set.null . inputStates

ftaToFwaTransition :: Fta.Transition s -> Fwa.Transition s
ftaToFwaTransition (Fta.Transition label inputStates finalState) =
  Fwa.Transition label (elemAt 0 inputStates) finalState

startStateFromTransitions :: Set (Fta.Transition s) -> Maybe s
startStateFromTransitions transitions =
  case findSingle (Set.null . inputStates) transitions of
    Just x -> Just (Fta.finalState x)
    _ -> Nothing


