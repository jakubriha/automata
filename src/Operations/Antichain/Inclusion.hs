module Operations.Antichain.Inclusion
  ( isSubsetOf
  , post'
  ) where

import Types.Fa (Fa(..), symbols, Transition, finalState)
import qualified Types.Fa as Fa (Transition(..)) 
import Operations.Regular (isMacrostateAccepting, postForEachSymbol)
import qualified Helpers (isSubsetOf, none, remove)
import Data.List (union)
import Control.Monad.State

isLowerOrEqual :: Eq sta => [sta] -> [sta] -> Bool
isLowerOrEqual =
  Helpers.isSubsetOf

post' :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym sta -> ProductState sta -> [ProductState sta]
post' fa1 fa2 (p, p') =
  [ (r, [ finalState tran | tran <- transitions fa2, stat <- p', isApplicableTransition a stat tran])
  | a <- symbols fa1 `union` symbols fa2
  , r <- (fmap finalState . filter (isApplicableTransition a p)) (transitions fa1)
  ]
    where
      isApplicableTransition symbol state (Fa.Transition symbol' state' _) =
        symbol == symbol' && state == state'

type ProductState sta = (sta, [sta])
type InnerState sta = ([ProductState sta], [ProductState sta])
type Post sta = ProductState sta -> [ProductState sta]
type IsAccepting sta = ProductState sta -> Bool

isAccepting :: (Eq sta, Eq sta) => Fa sym sta -> Fa sym sta -> ProductState sta -> Bool
isAccepting (Fa _ finalStates1 _) fa2 (p, r) =
  p `elem` finalStates1 && not (isMacrostateAccepting fa2 r)

isSubsetOf :: (Eq sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1 fa2 =
  not (any (isAccepting fa1 fa2) next) && while'
    where
      next = [(p, initialStates fa2) | p <- initialStates fa1]
      while' =
        evalState (while (post' fa1 fa2) (isAccepting fa1 fa2)) ([], next)

moveRFromNextToProcessed :: State (InnerState sta) (ProductState sta)
moveRFromNextToProcessed = state $ \(processed, r : next') ->
  (r, (r : processed, next'))

while :: (Eq sta, Eq sta) => Post sta -> IsAccepting sta -> State (InnerState sta) Bool
while post isAccepting = do
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

processPostStates :: Eq sta => [ProductState sta] -> IsAccepting sta -> State (InnerState sta) Bool
processPostStates [] _ =
  return False
processPostStates (newProduct : ps) isAccepting =
  if isAccepting newProduct
    then return True
    else do
      (processed, next) <- get
      if not (uncurry elem newProduct) && Helpers.none (\(s, s') -> fst newProduct == s && (s' `isLowerOrEqual` snd newProduct)) (processed `union` next)
        then do
          removeFromStateAllGreaterThan newProduct
          addToNext newProduct
          processPostStates ps isAccepting
        else processPostStates ps isAccepting

removeFromStateAllGreaterThan :: Eq sta => ProductState sta -> State (InnerState sta) ()
removeFromStateAllGreaterThan p = do
  (processed, next) <- get
  state $ \(processed, next) ->
    let
      predicate (s, s') = s == fst p && s' `isLowerOrEqual` snd p
      processed' = Helpers.remove predicate processed
      next' = Helpers.remove predicate next
    in
      ((), (processed', next'))

addToNext :: ProductState sta -> State (InnerState sta) ()
addToNext p =
  state $ \(processed, next) ->
    ((), (processed, p : next))
