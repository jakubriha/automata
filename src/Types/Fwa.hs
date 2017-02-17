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

data Transition i s =
  Transition
    { label :: i
    , state :: s
    , finalState :: s
    } deriving (Eq, Ord)

data Fwa i s =
  Fwa
    { startStates :: [s]
    , finalStates :: [s]
    , transitions :: [Transition i s]
    }

states :: (Eq s) => Fwa i s -> [s]
states (Fwa startStates finalStates transitions) =
  nub (startStates ++ finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

labels :: Eq i => Fwa i s -> [i]
labels (Fwa _ _ transitions) =
  nub (fmap label transitions)

instance (Show i, Show s) => Show (Transition i s) where
  show (Transition label state finalState) =
    show label ++ "(" ++ show state ++ ") -> " ++ show finalState

instance (Eq i, Show i, Eq s, Show s) => Show (Fwa i s) where
  show fwa =
    printLabelList (labels fwa) ++ "\n\n" ++ printAutomaton fwa

printLabelList :: Show i => [i] -> String
printLabelList labels =
  "Ops " ++ printLabelDecl "x" ++ " " ++ (unwords . fmap printLabelDecl) labels

printLabelDecl :: Show i => i -> String
printLabelDecl label =
  show label ++ ":1"

printAutomaton :: (Show i, Eq s, Show s) => Fwa i s -> String
printAutomaton fwa =
  "Automaton A\n"
  ++ "States " ++ unwords (show <$> states fwa) ++ "\n"
  ++ "Final States " ++ unwords (show <$> finalStates fwa) ++ "\n"
  ++ "Transitions\n"
  ++ printStartStates (startStates fwa) ++ "\n"
  ++ (intercalate "\n" . fmap show) (transitions fwa)

printStartStates :: Show s => [s] -> String
printStartStates =
  intercalate "\n" . fmap (("x -> " ++) . show)

