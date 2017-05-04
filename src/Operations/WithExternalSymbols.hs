{-# LANGUAGE MonadComprehensions #-}

{-|
Module      : Operations.WithExternalSymbols
Description : Contains FA operations. Each operation has one extra parameter to
              pass an external alphabet which is used instead of the implicit
              alphabet.
-}
module Operations.WithExternalSymbols
  ( isMacrostateAccepting
  , post
  , postForEachSymbol
  ) where

import Types.Fa hiding (State, state, symbols)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Loops (whileM_)

-- |Converts String to a list of symbols.
charsToSymbols :: String -> [Symbol]
charsToSymbols =
  fmap (: [])

-- |Determines whether a macro-state is accepting.
isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool
isMacrostateAccepting fa states =
  states `Set.intersection` finalStates fa /= Set.empty

-- |Returns the post state of a state and a specific symbol.
post :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> sym -> Set sta
post fa currentStates symbol =
  Set.map target $ Set.filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol source _) =
        tSymbol == symbol && source `elem` currentStates

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set sym -> Set (Set sta)
postForEachSymbol fa state =
  Set.map (post fa state)
