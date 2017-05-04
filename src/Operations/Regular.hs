{-|
Module      : Operations.WithExternalSymbols
Description : Contains all FA operations. Each operation uses the implicit
              alphabet.
-}
module Operations.Regular
  ( charsToSymbols
  , isMacrostateAccepting
  , run
  , module Operations.WithExternalSymbols
  , postForEachSymbol
  ) where

import Types.Fa
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List ((\\), nub)
import Operations.WithExternalSymbols (post)
import qualified Operations.WithExternalSymbols as ExternalSymbols

-- |Converts 'String' to a list of symbols.
charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

-- |Determines whether a macro-state is accepting.
isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool 
isMacrostateAccepting fa states = 
  not $ Set.null $ states `Set.intersection` finalStates fa

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set (Set sta)
postForEachSymbol fa state =
  ExternalSymbols.postForEachSymbol fa state (symbols fa)

-- |Checks whether a FA accepts a string.
run :: (Ord sym, Ord sta) => Fa sym sta -> [sym] -> Bool
run fa =
  run' (initialStates fa)
    where
      run' currentStates [] =
        isMacrostateAccepting fa currentStates
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs
