module Parsing.General where

import qualified Data.ByteString as B
import Types.Fta as Fta
import Types.Fwa as Fwa
import Parsing.Fta (file)
import Text.Parsec (runParser, ParseError)
import Text.Parsec.Error (Message(..), newErrorMessage)
import Text.Parsec.Pos (initialPos)
import Data.ByteString (ByteString)
import Data.Set as Set

parseFta :: ByteString -> Either ParseError Fta
parseFta = runParser file () ""

loadAndParseFta :: FilePath -> IO (Either ParseError Fta)
loadAndParseFta filePath = fmap parseFta (B.readFile filePath)

loadAndParseFwa :: FilePath -> IO (Either ParseError Fwa)
loadAndParseFwa filePath = fmap parseFwa (B.readFile filePath)

parseFwa :: ByteString -> Either ParseError Fwa
parseFwa fileContent = case parseFta fileContent of
                        Left error -> Left error
                        Right fta -> case ftaToFwa fta of
                                         Just fwa -> Right fwa
                                         _        -> Left $ newErrorMessage (Message "Cannot convert FTA to FWA") (initialPos "")

ftaToFwa :: Fta -> Maybe Fwa
ftaToFwa (Fta states finalStates transitions rankedAlphabet) =
    case startStateFromTransitions transitions of
      Nothing -> Nothing
      Just startState ->
        let fwaTransitions = Set.map fwaTransition transitions
            alphabet       = Set.map fst rankedAlphabet
        in
            Just (Fwa states startState finalStates fwaTransitions alphabet)

startStateFromTransitions :: Set Fta.Transition -> Maybe Fwa.State
startStateFromTransitions transitions =
    case findSingle (\transition -> Set.null (inputStates transition)) transitions of
      Just x -> Just $ elemAt 0 (inputStates x)
      _      -> Nothing

fwaTransition :: Fta.Transition -> Fwa.Transition
fwaTransition (Fta.Transition label inputStates finalState) =
    Fwa.Transition label (elemAt 0 inputStates) finalState

findSingle :: (a -> Bool) -> Set a -> Maybe a
findSingle predicate set =
    case Set.toList $ Set.filter predicate set of
      [x] -> Just x
      _   -> Nothing

