module Operations.Regular
  ( isMacrostateAccepting
  , postForEachSymbol
  , union
  ) where

import Types.Fa
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Data.List ((\\), nub)
import qualified Operations.WithExternalSymbols as ExternalSymbols

isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool 
isMacrostateAccepting fa states = 
  states `Set.intersection` finalStates fa /= Set.empty

postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set (Set sta)
postForEachSymbol fa = 
  ExternalSymbols.postForEachSymbol (symbols fa) fa

union :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `Set.union` initialStates2)
    (finalStates1 `Set.union` finalStates2)
    (transitions1 `Set.union` transitions2)
