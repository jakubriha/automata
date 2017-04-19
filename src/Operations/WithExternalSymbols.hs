module Operations.WithExternalSymbols
  ( postForEachSymbol
  ) where

import Types.Fa hiding (State, state, symbols)
import qualified Data.List as List
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Data.List ((\\), nub)
import Control.Monad.State
import Control.Monad.Loops (whileM_)

charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool
isMacrostateAccepting fa states =
  states `Set.intersection` finalStates fa /= Set.empty

post :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> sym -> Set sta
post fa currentStates symbol =
  fmap finalState $ Set.filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

postForEachSymbol :: (Ord sym, Ord sta) => Set sym -> Fa sym sta -> Set sta -> Set (Set sta)
postForEachSymbol symbols fa state =
  fmap (post fa state) symbols
