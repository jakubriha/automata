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

data Transition sym sta =
  Transition
    { symbol :: sym
    , state :: sta
    , finalState :: sta
    } deriving (Eq, Ord)

data Fa sym sta =
  Fa
    { initialStates :: [sta]
    , finalStates :: [sta]
    , transitions :: [Transition sym sta]
    }

instance Functor (Fa sym) where
  fmap f fa =
    let
      initial = (fmap f . initialStates) fa
      final = (fmap f . finalStates) fa
      transitionMapper (Transition symbol state finalState) =
        Transition symbol (f state) (f finalState)
      trans = (fmap transitionMapper . transitions) fa
    in
      Fa initial final trans

states :: (Eq sta) => Fa sym sta -> [sta]
states (Fa initialStates finalStates transitions) =
  nub (initialStates ++ finalStates ++ concatMap mapper transitions)
    where
      mapper (Transition _ state finalState) = [state, finalState]

symbols :: Eq sym => Fa sym sta -> [sym]
symbols (Fa _ _ transitions) =
  nub (fmap symbol transitions)

instance (Show sym, Show sta) => Show (Transition sym sta) where
  show (Transition symbol state finalState) =
    show symbol ++ "(" ++ show state ++ ") -> " ++ show finalState

instance (Eq sym, Show sym, Eq sta, Show sta) => Show (Fa sym sta) where
  show fa =
    printSymbolList (symbols fa) ++ "\n\n" ++ printAutomaton fa

printSymbolList :: Show sym => [sym] -> String
printSymbolList symbols =
  "Ops " ++ printSymbolDecl "x" ++ " " ++ (unwords . fmap printSymbolDecl) symbols

printSymbolDecl :: Show sym => sym -> String
printSymbolDecl symbol =
  show symbol ++ ":1"

printAutomaton :: (Show sym, Eq sta, Show sta) => Fa sym sta -> String
printAutomaton fa =
  "Automaton A\n"
  ++ "States " ++ unwords (show <$> states fa) ++ "\n"
  ++ "Final States " ++ unwords (show <$> finalStates fa) ++ "\n"
  ++ "Transitions\n"
  ++ printInitialStates (initialStates fa) ++ "\n"
  ++ (intercalate "\n" . fmap show) (transitions fa)

printInitialStates :: Show sta => [sta] -> String
printInitialStates =
  intercalate "\n" . fmap (("x -> " ++) . show)

