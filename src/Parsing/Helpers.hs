module Parsing.Helpers
  ( findSingle
  , parseErrorToString
  ) where

import Text.Parsec (ParseError)
import Text.Parsec.Error (errorPos, errorMessages, messageString)
import Text.Parsec.Pos (sourceName, sourceLine, sourceColumn)
import Data.List (intercalate)
import Data.Set as Set

parseErrorToString :: ParseError -> String
parseErrorToString parseError = (intercalate ":" . fmap ($ parseError)) [getName, getLine, getColumn, getMessage]
  where
    getName = show . sourceName . errorPos
    getLine = show . sourceLine . errorPos
    getColumn = show . sourceColumn . errorPos
    getMessage = concatMap messageString . errorMessages

findSingle :: (a -> Bool) -> Set.Set a -> Maybe a
findSingle predicate set =
  case Set.toList $ Set.filter predicate set of
    [x] -> Just x
    _   -> Nothing
