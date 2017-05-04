{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Types.Fa
  ( Symbol
  , State
  , Transition (..)
  , Fa (..)
  , states
  , symbols
  ) where

import Data.List (intercalate)
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

type Symbol =
  String

type State =
  String

-- |Represents a transition of a FA with symbol 'sym' and states 'sta'.
data Transition sym sta =
  Transition
    { symbol :: sym
    , source :: sta
    , target :: sta
    } deriving (Eq, Ord, NFData, Generic)

-- |Represents a finite automaton (FA) with symbols 'sym' and states 'sta'.
data Fa sym sta =
  Fa
    { initialStates :: Set sta
    , finalStates :: Set sta
    , transitions :: Set (Transition sym sta)
    } deriving (NFData, Generic)

-- | Returns states of a FA.
states :: (Ord sta) => Fa sym sta -> Set sta
states (Fa initialStates finalStates transitions) =
  Set.unions [initialStates, finalStates, Set.fromList $ concatMap mapper transitions]
    where
      mapper (Transition _ source target) = [source, target]

-- | Returns alphabet of a FA.
symbols :: Ord sym => Fa sym sta -> Set sym
symbols (Fa _ _ transitions) =
  fmap symbol transitions

instance (Show sym, Show sta) => Show (Transition sym sta) where
  show (Transition symbol source target) =
    show symbol ++ "(" ++ show source ++ ") -> " ++ show target

-- | Returns string representation of a FA in the Timbuk file format.
instance (Ord sym, Show sym, Ord sta, Show sta) => Show (Fa sym sta) where
  show fa =
    printSymbolList (symbols fa) ++ "\n\n" ++ printAutomaton fa

printSymbolList :: (Ord sym, Show sym) => Set sym -> String
printSymbolList symbols =
  "Ops " ++ printSymbolDecl "x" ++ " " ++ (unwords . fmap printSymbolDecl) (Set.toList symbols)

printSymbolDecl :: Show sym => sym -> String
printSymbolDecl symbol =
  show symbol ++ ":1"

printAutomaton :: (Ord sym, Show sym, Ord sta, Show sta) => Fa sym sta -> String
printAutomaton fa =
  "Automaton A\n"
  ++ "States " ++ unwords (show <$> Set.toList (states fa)) ++ "\n"
  ++ "Final States " ++ unwords (show <$> Set.toList (finalStates fa)) ++ "\n"
  ++ "Transitions\n"
  ++ printInitialStates (initialStates fa) ++ "\n"
  ++ (intercalate "\n" . fmap show) (Set.toList $ transitions fa)

printInitialStates :: (Ord sta, Show sta) => Set sta -> String
printInitialStates states =
  (intercalate "\n" . fmap (("x -> " ++) . show)) (Set.toList states)
