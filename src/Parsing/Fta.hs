module Parsing.Fta
  ( parseFta
  ) where

import Data.ByteString (ByteString)
import Text.Parsec hiding (State)
import Text.Parsec.ByteString
import Data.Set (Set, fromList, empty)
import Text.Parsec.Char
import Text.Parsec.Number

import Types.Fta
import Parsing.Helpers (parseErrorToString)

parseFta :: (Monad m) => ByteString -> m Fta
parseFta fileContent =
  case runParser file () "" fileContent of
    Left error -> fail (parseErrorToString error)
    Right fta -> return fta

file :: Parser Fta
file = do
  string "Ops "
  symbolList <- symbolList
  skipMany1 endOfLine
  (states, finalStates, transitions) <- automaton
  returnFta states finalStates transitions symbolList

returnFta :: Set State -> Set State -> Set Transition -> RankedAlphabet -> Parser Fta
returnFta states finalStates transitions rankedAlphabet =
  case makeFta states finalStates transitions rankedAlphabet of
    Just fta -> return fta
    _ -> fail "Invalid FTA"

symbolList :: Parser RankedAlphabet
symbolList =
  sepEndByToSet symbolDecl (char ' ')

symbolDecl :: Parser (Symbol, Rank)
symbolDecl = do
  symbol <- Parsing.Fta.symbol
  char ':'
  rank <- decimal
  return (symbol, rank)
  
automaton :: Parser (Set String, Set String, Set Transition)
automaton = do
  string "Automaton "
  many1 alphaNum
  skipSpacesAndEol
  string "States "
  states <- stateList
  skipSpacesAndEol
  string "Final States "
  finalStates <- stateList
  skipSpacesAndEol
  string "Transitions"
  endOfLine
  transitions <- transitionList
  return (states, finalStates, transitions)

stateList :: Parser (Set String)
stateList =
  sepEndByToSet state (char ' ')

state :: Parser String
state =
  many1 alphaNum

transitionList :: Parser (Set Transition)
transitionList =
  sepEndByToSet transition endOfLine

transition :: Parser Transition
transition = do
  symbol <- Parsing.Fta.symbol
  inputStates <- transitionStateList
  string " -> "
  finalState <- state
  return (Types.Fta.Transition symbol inputStates finalState)

transitionStateList :: Parser (Set String)
transitionStateList =
  option empty (brackets (sepEndByToSet state (char ',')))

symbol :: Parser Symbol
symbol =
  many1 alphaNum

sepEndByToSet :: (Ord a, Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (Set a)
sepEndByToSet value separator =
  fmap fromList (value `sepEndBy` separator)

skipSpacesAndEol :: Parser ()
skipSpacesAndEol = do
  skipMany (char ' ')
  endOfLine; return ()

brackets =
  between (char '(') (char ')')
