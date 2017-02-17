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

data Transition s =
  Transition
    { label :: Label
    , state :: s
    , finalState :: s
    } deriving (Eq, Ord)

data Fwa s =
  Fwa
    { startStates :: [s]
    , finalStates :: [s]
    , transitions :: [Transition s]
    }

states :: (Eq s) => Fwa s -> [s]
states (Fwa startStates finalStates transitions) =
  nub (startStates ++ finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

labels :: Fwa s -> [Label]
labels (Fwa _ _ transitions) =
  nub (fmap label transitions)

instance Show s => Show (Transition s) where
  show (Transition label state finalState) =
    label ++ "(" ++ show state ++ ") -> " ++ show finalState

instance (Eq s, Show s) => Show (Fwa s) where
  show fwa =
    printLabelList (labels fwa) ++ "\n\n" ++ printAutomaton fwa

printLabelList :: [Label] -> String
printLabelList labels =
  "Ops " ++ (unwords . fmap printLabelDecl) (labels ++ ["x"])

printLabelDecl :: Label -> String
printLabelDecl label =
  label ++ ":1"

printAutomaton :: (Eq s, Show s) => Fwa s -> String
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

