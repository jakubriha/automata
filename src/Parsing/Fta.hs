module Parsing.Fta
  ( parseFta
  ) where

import Data.ByteString (ByteString)
import Text.Parsec
import Text.Parsec.ByteString
import Data.Set (Set, fromList, empty)
import Text.Parsec.Char
import Text.Parsec.Number

import Types.Fta
import Parsing.Helpers (parseErrorToString)

parseFta :: (Monad m) => ByteString -> m (Fta String)
parseFta fileContent =
  case runParser file () "" fileContent of
    Left error -> fail (parseErrorToString error)
    Right fta -> return fta

file :: Parser (Fta String)
file = do
  { string "Ops "
  ; labelList <- labelList
  ; skipMany1 endOfLine
  ; (states, finalStates, transitions) <- automaton
  ; returnFta states finalStates transitions labelList
  }

returnFta :: Ord s => Set s -> Set s -> Set (Transition s) -> RankedAlphabet -> Parser (Fta s)
returnFta states finalStates transitions rankedAlphabet =
  case makeFta states finalStates transitions rankedAlphabet of
    Just fta -> return fta
    _ -> fail "Invalid FTA"

labelList :: Parser RankedAlphabet
labelList =
  sepEndByToSet labelDecl (char ' ')

labelDecl :: Parser (Label, Rank)
labelDecl = do
  { label <- Parsing.Fta.label
  ; char ':'
  ; rank <- decimal
  ; return (label, rank)
  }

automaton :: Parser (Set String, Set String, Set (Transition String))
automaton = do
  { string "Automaton "
  ; many1 alphaNum
  ; skipSpacesAndEol
  ; string "States "
  ; states <- stateList
  ; skipSpacesAndEol
  ; string "Final States "
  ; finalStates <- stateList
  ; skipSpacesAndEol
  ; string "Transitions"
  ; endOfLine
  ; transitions <- transitionList
  ; return (states, finalStates, transitions)
  }

stateList :: Parser (Set String)
stateList =
  sepEndByToSet state (char ' ')

state :: Parser String
state =
  many1 alphaNum

transitionList :: Parser (Set (Transition String))
transitionList =
  sepEndByToSet transition endOfLine

transition :: Parser (Transition String)
transition = do
  { label <- Parsing.Fta.label
  ; inputStates <- transitionStateList
  ; string " -> "
  ; finalState <- state
  ; return (Types.Fta.Transition label inputStates finalState)
  }

transitionStateList :: Parser (Set String)
transitionStateList =
  option empty (brackets (sepEndByToSet state (char ',')))

label :: Parser Label
label =
  many1 alphaNum

sepEndByToSet :: (Ord a, Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m (Set a)
sepEndByToSet value separator =
  fmap fromList (value `sepEndBy` separator)

skipSpacesAndEol :: Parser ()
skipSpacesAndEol = do
  { skipMany (char ' ')
  ; endOfLine; return ()
  }

brackets =
  between (char '(') (char ')')

