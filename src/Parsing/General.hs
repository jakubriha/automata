module Parsing.General
  ( loadFta
  , loadFa
  , parseFa
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Set (Set)
import qualified Data.Set as Set

import Types.Fta as Fta
import Types.Fa as Fa
import Parsing.Fta (parseFta)

-- |Returns a FTA parsed from a file specified by its filename. Possible failure is represented by the 'm' type.
loadFta :: (Monad m) => FilePath -> IO (m Fta)
loadFta filePath =
  fmap parseFta (B.readFile filePath)

-- |Returns a FA parsed from a file specified by its filename. Possible failure is represented by the 'm' type.
loadFa :: (Monad m) => FilePath -> IO (m (Fa Fa.Symbol Fa.State))
loadFa filePath =
  fmap parseFa (B.readFile filePath)

-- |Returns a FA parsed from a string. Possible failure is represented by the 'm' type.
parseFa :: (Monad m) => ByteString -> m (Fa Fa.Symbol Fa.State)
parseFa fileContent =
  case parseFta fileContent of
    Left error -> fail error
    Right fta -> return $ ftaToFa fta

-- |Converts a FTA to a FA.
ftaToFa :: Fta -> Fa Fa.Symbol Fa.State
ftaToFa (Fta states finalStates transitions rankedAlphabet) =
  Fa (extractInitialStates transitions) finalStates faTransitions
    where
      faTransitions = (Set.map ftaToFaTransition . Set.filter isNotInitialTransition) transitions

isNotInitialTransition :: Fta.Transition -> Bool
isNotInitialTransition =
  not . Set.null . inputStates

ftaToFaTransition :: Fta.Transition -> Fa.Transition Fa.Symbol Fa.State
ftaToFaTransition (Fta.Transition symbol inputStates finalState) =
  Fa.Transition symbol (head . Set.elems $ inputStates) finalState

extractInitialStates :: Set.Set Fta.Transition -> Set Fta.State
extractInitialStates =
  Set.map Fta.finalState . Set.filter (Set.null . inputStates)
