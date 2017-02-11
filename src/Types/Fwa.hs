module Types.Fwa
  ( Label
  , State
  , Transition (..)
  , Fwa (..)
  , states
  , labels
  ) where

import Data.List (nub, intersperse, intercalate)

type Label =
  String

type State =
  String

data Transition =
  Transition
    { label :: Label
    , state :: State
    , finalState :: State
    } deriving (Eq, Ord)

data Fwa =
  Fwa
    { startStates :: [State]
    , finalStates :: [State]
    , transitions :: [Transition]
    }

states :: Fwa -> [State]
states (Fwa startStates finalStates transitions) =
  nub (startStates ++ finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

labels :: Fwa -> [Label]
labels (Fwa _ _ transitions) =
  nub (fmap label transitions)

instance Show Transition where
  show (Transition label state finalState) =
    label ++ "(" ++ state ++ ") -> " ++ finalState

instance Show Fwa where
  show fwa =
    printLabelList (labels fwa) ++ "\n\n" ++ printAutomaton fwa

printLabelList :: [Label] -> String
printLabelList labels =
  "Ops " ++ (unwords . fmap printLabelDecl) (labels ++ ["x"])

printLabelDecl :: Label -> String
printLabelDecl label =
  label ++ ":1"

printAutomaton :: Fwa -> String
printAutomaton fwa =
  "Automaton A\n"
  ++ "States " ++ unwords (states fwa) ++ "\n"
  ++ "Final States " ++ unwords (finalStates fwa) ++ "\n"
  ++ "Transitions\n"
  ++ printStartStates (startStates fwa) ++ "\n"
  ++ (intercalate "\n" . fmap show) (transitions fwa)

printStartStates :: [State] -> String
printStartStates =
  intercalate "\n" . fmap (\state -> "x -> " ++ state)

