{-# LANGUAGE MonadComprehensions, BangPatterns #-}

module Operations.Antichain.Inclusion
  ( isSubsetOf
  , post'
  ) where

import Types.Fa (Fa(..), symbols, Transition, finalState)
import qualified Types.Fa as Fa (Transition(..)) 
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Operations.Regular (isMacrostateAccepting)
import qualified Helpers (isSubsetOf, none, remove)
import Control.Monad.State

isLowerOrEqual :: Ord sta => Set sta -> Set sta -> Bool
isLowerOrEqual =
  Helpers.isSubsetOf

post' :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> ProductState sta -> Set (ProductState sta)
post' !fa1 !fa2 (!p, !p') =
  [ (r, [ finalState tran | tran <- transitions fa2, stat <- p', isApplicableTransition a stat tran])
  | a <- symbols fa1 `Set.union` symbols fa2
  , r <- (fmap finalState . Set.filter (isApplicableTransition a p)) (transitions fa1)
  ]
    where
      isApplicableTransition symbol state (Fa.Transition symbol' state' _) =
        symbol == symbol' && state == state'

type ProductState sta = (sta, Set sta)
type InnerState sta = (Set (ProductState sta), Set (ProductState sta))
type Post sta = ProductState sta -> Set (ProductState sta)
type IsAccepting sta = ProductState sta -> Bool

isAccepting :: (Ord sym, Ord sta, Eq sta) => Fa sym sta -> Fa sym sta -> ProductState sta -> Bool
isAccepting (Fa _ !finalStates1 _) !fa2 !(p, r) =
  p `elem` finalStates1 && not (isMacrostateAccepting fa2 r)

isSubsetOf :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf !fa1 !fa2 =
  not (any (isAccepting fa1 fa2) next) && while'
    where
      next = [(p, initialStates fa2) | p <- initialStates fa1]
      while' =
        evalState (while (post' fa1 fa2) (isAccepting fa1 fa2)) (Set.empty, next)

moveRFromNextToProcessed :: Ord sta => State (InnerState sta) (ProductState sta)
moveRFromNextToProcessed = state $ \(!processed, !next) ->
  let
    r = Set.findMin next
    processed' = Set.insert r processed
    next' = Set.delete r next
  in
    (r, (processed', next'))

while :: (Ord sta, Eq sta) => Post sta -> IsAccepting sta -> State (InnerState sta) Bool
while !post !isAccepting = do
  (_, next) <- get
  if not $ null next
    then do
      r <- moveRFromNextToProcessed
      shouldExit <- processPostStates (post r) isAccepting
      if shouldExit
        then return False
        else while post isAccepting
    else
      return True

processPostStates :: Ord sta => Set (ProductState sta) -> IsAccepting sta -> State (InnerState sta) Bool
processPostStates !ps !isAccepting
  | ps == Set.empty = return False
  | otherwise =
    let
      newProduct = Set.findMin ps
      ps' = Set.delete newProduct ps
    in
      if isAccepting newProduct
        then return True
        else do
          (processed, next) <- get
          if not (uncurry elem newProduct) && Helpers.none (\(s, s') -> fst newProduct == s && (s' `isLowerOrEqual` snd newProduct)) (processed `Set.union` next)
            then do
              removeFromStateAllGreaterThan newProduct
              addToNext newProduct
              processPostStates ps' isAccepting
            else
              processPostStates ps' isAccepting

removeFromStateAllGreaterThan :: Ord sta => ProductState sta -> State (InnerState sta) ()
removeFromStateAllGreaterThan !p = do
  (processed, next) <- get
  state $ \(!processed, !next) ->
    let
      predicate (s, s') = s == fst p && s' `isLowerOrEqual` snd p
      processed' = Helpers.remove predicate processed
      next' = Helpers.remove predicate next
    in
      ((), (processed', next'))

addToNext :: Ord sta => ProductState sta -> State (InnerState sta) ()
addToNext p =
  state $ \(!processed, !next) ->
    ((), (processed, next `Set.union` Set.singleton p))
