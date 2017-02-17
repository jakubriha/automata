module Types.Fa
  ( Label
  , State
  , Transition (..)
  , Fa (..)
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

labels :: Eq i => Fa i s -> [i]
labels (Fa _ _ transitions) =
  nub (fmap label transitions)

instance (Show i, Show s) => Show (Transition i s) where
  show (Transition label state finalState) =
    show label ++ "(" ++ show state ++ ") -> " ++ show finalState

instance (Eq i, Show i, Eq s, Show s) => Show (Fa i s) where
  show fa =
    printLabelList (labels fa) ++ "\n\n" ++ printAutomaton fa

printLabelList :: Show i => [i] -> String
printLabelList labels =
  "Ops " ++ printLabelDecl "x" ++ " " ++ (unwords . fmap printLabelDecl) labels

printLabelDecl :: Show i => i -> String
printLabelDecl label =
  show label ++ ":1"

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

