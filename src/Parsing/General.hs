module Parsing.General
  ( loadFta
  , loadFa
  ) where

import Data.ByteString as B

import Data.Set as Set

import Types.Fta as Fta
import Types.Fa as Fa
import Parsing.Fta (parseFta)
import Helpers (findSingle, findSingleInSet)

loadFta :: (Monad m) => FilePath -> IO (m Fta)
loadFta filePath =
  fmap parseFta (B.readFile filePath)

loadFa :: (Monad m) => FilePath -> IO (m (Fa Fa.Symbol Fa.State))
loadFa filePath =
  fmap parseFa (B.readFile filePath)

parseFa :: (Monad m) => B.ByteString -> m (Fa Fa.Symbol Fa.State)
parseFa fileContent =
  case parseFta fileContent of
    Left error -> fail error
    Right fta -> case ftaToFa fta of
      Just fa -> return fa
      _ -> error "Cannot convert FTA to fa"

ftaToFa :: Fta -> Maybe (Fa Fa.Symbol Fa.State)
ftaToFa (Fta states finalStates transitions rankedAlphabet) =
  fmap mapper (findInitialStateIn transitions)
    where
      faTransitions = (Set.map ftaToFaTransition . Set.filter isNotInitialTransition) transitions
      mapper initialState = Fa [initialState] (Set.toList finalStates) (Set.toList faTransitions)

isNotInitialTransition :: Fta.Transition -> Bool
isNotInitialTransition = not . Set.null . inputStates

ftaToFaTransition :: Fta.Transition -> Fa.Transition Fa.Symbol Fa.State
ftaToFaTransition (Fta.Transition symbol inputStates finalState) =
  Fa.Transition symbol (elemAt 0 inputStates) finalState

findInitialStateIn :: Set Fta.Transition -> Maybe Fta.State
findInitialStateIn transitions =
  case findSingleInSet (Set.null . inputStates) transitions of
    Just x -> Just (Fta.finalState x)
    _ -> Nothing

