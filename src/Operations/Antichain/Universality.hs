{-# LANGUAGE MonadComprehensions #-}

module Operations.Antichain.Universality
  ( isUniversal
  ) where

import Types.Fa (Fa, initialStates, symbols)
import Operations.Regular (isMacrostateAccepting, postForEachSymbol)
import qualified Helpers (isSubsetOf)
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Control.Monad.State

isLowerOrEqual :: Ord sta => Set sta -> Set sta -> Bool
isLowerOrEqual =
  Helpers.isSubsetOf

type MacroState sta = Set sta
type InnerState sta = (Set (MacroState sta), Set (MacroState sta))
type Post sta = MacroState sta -> Set (MacroState sta)
type IsRejecting sta = MacroState sta -> Bool

-- |Checks whether a FA accepts all possible strings using the antichain-based algorithm.
isUniversal:: (Ord sym, Ord sta) => Fa sym sta -> Bool
isUniversal fa =
  isMacrostateAccepting fa (initialStates fa) && while'
    where
      while' =
        evalState (while (postForEachSymbol fa) (not . isMacrostateAccepting fa)) (Set.empty, Set.singleton $ initialStates fa)

moveRFromNextToProcessed :: Ord sta => State (InnerState sta) (MacroState sta)
moveRFromNextToProcessed = state $ \(processed, next) ->
  let
    r = Set.findMin next
    processed' = Set.insert r processed
    next' = Set.delete r next
  in
    (r, (processed', next'))

while :: Ord sta => Post sta -> IsRejecting sta -> State (InnerState sta) Bool
while post isRejecting = do
  (_, next) <- get
  if not $ null next
    then do
      r <- moveRFromNextToProcessed
      shouldExit <- processPostStates (post r) isRejecting
      if shouldExit
        then return False
        else while post isRejecting
    else return True

processPostStates :: Ord sta => Set (MacroState sta) -> IsRejecting sta -> State (InnerState sta) Bool
processPostStates ps isRejecting
  | ps == Set.empty = return False
  | otherwise =
    let
        newProduct = Set.findMin ps
        ps' = Set.delete newProduct ps
    in
      if isRejecting newProduct
        then return True
        else do
        (processed, next) <- get
        if not $ or [s `isLowerOrEqual` newProduct | s <- processed `Set.union` next]
            then do
            removeFromStateAllGreaterThan newProduct
            addToNext newProduct
            processPostStates ps' isRejecting
            else processPostStates ps' isRejecting

removeFromStateAllGreaterThan :: Ord sta => MacroState sta -> State (InnerState sta) ()
removeFromStateAllGreaterThan p = do
  (processed, next) <- get
  state $ \(processed, next) ->
    let
      processed' = processed Set.\\ [s | s <- processed, p `isLowerOrEqual` s ]
      next' = next Set.\\ [s | s <- next, p `isLowerOrEqual` s ]
    in
      ((), (processed', next'))

addToNext :: Ord sta => MacroState sta -> State (InnerState sta) ()
addToNext p =
  state $ \(processed, next) ->
    ((), (processed, Set.insert p next))
