module Parsing.General
  ( loadFta
  , loadFa
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import qualified Data.Set as Set

import Types.Fta as Fta
import Types.Fa as Fa
import Parsing.Fta (parseFta)

loadFta :: (Monad m) => FilePath -> IO (m Fta)
loadFta filePath =
  fmap parseFta (B.readFile filePath)

loadFa :: (Monad m) => FilePath -> IO (m (Fa Fa.Symbol Fa.State))
loadFa filePath =
  fmap parseFa (B.readFile filePath)

parseFa :: (Monad m) => ByteString -> m (Fa Fa.Symbol Fa.State)
parseFa fileContent =
  case parseFta fileContent of
    Left error -> fail error
    Right fta -> return $ ftaToFa fta

ftaToFa :: Fta -> Fa Fa.Symbol Fa.State
ftaToFa (Fta states finalStates transitions rankedAlphabet) =
  Fa (extractInitialStates transitions) (Set.toList finalStates) (Set.toList faTransitions)
    where
      faTransitions = (Set.map ftaToFaTransition . Set.filter isNotInitialTransition) transitions

isNotInitialTransition :: Fta.Transition -> Bool
isNotInitialTransition = not . Set.null . inputStates

ftaToFaTransition :: Fta.Transition -> Fa.Transition Fa.Symbol Fa.State
ftaToFaTransition (Fta.Transition symbol inputStates finalState) =
  Fa.Transition symbol (Set.elemAt 0 inputStates) finalState

extractInitialStates :: Set.Set Fta.Transition -> [Fta.State]
extractInitialStates =
  fmap Fta.finalState . Set.toList . Set.filter (Set.null . inputStates)
