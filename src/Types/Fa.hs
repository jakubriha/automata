module Types.Fa
  ( Symbol
  , State
  , Transition (..)
  , Fa (..)
  , states
  , symbols
  ) where

import Data.List (nub, intersperse, intercalate)

type Symbol =
  String

type State =
  String

data Transition i s =
  Transition
    { symbol :: i
    , state :: s
    , finalState :: s
    } deriving (Eq, Ord)

data Fa i s =
  Fa
    { initialStates :: [s]
    , finalStates :: [s]
    , transitions :: [Transition i s]
    }

states :: (Eq s) => Fa i s -> [s]
states (Fa initialStates finalStates transitions) =
  nub (initialStates ++ finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

symbols :: Eq i => Fa i s -> [i]
symbols (Fa _ _ transitions) =
  nub (fmap symbol transitions)

instance (Show i, Show s) => Show (Transition i s) where
  show (Transition symbol state finalState) =
    show symbol ++ "(" ++ show state ++ ") -> " ++ show finalState

instance (Eq i, Show i, Eq s, Show s) => Show (Fa i s) where
  show fa =
    printSymbolList (symbols fa) ++ "\n\n" ++ printAutomaton fa

printSymbolList :: Show i => [i] -> String
printSymbolList symbols =
  "Ops " ++ printSymbolDecl "x" ++ " " ++ (unwords . fmap printSymbolDecl) symbols

printSymbolDecl :: Show i => i -> String
printSymbolDecl symbol =
  show symbol ++ ":1"

printAutomaton :: (Show i, Eq s, Show s) => Fa i s -> String
printAutomaton fa =
  "Automaton A\n"
  ++ "States " ++ unwords (show <$> states fa) ++ "\n"
  ++ "Final States " ++ unwords (show <$> finalStates fa) ++ "\n"
  ++ "Transitions\n"
  ++ printInitialStates (initialStates fa) ++ "\n"
  ++ (intercalate "\n" . fmap show) (transitions fa)

printInitialStates :: Show s => [s] -> String
printInitialStates =
  intercalate "\n" . fmap (("x -> " ++) . show)

