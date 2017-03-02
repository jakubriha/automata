module Operations.Antichain
  ( isUniversal
  , while
  ) where

import Types.Fa (Fa, initialStates, symbols)
import Operations.Regular (isMacrostateAccepting, post)
import Data.List (union, intersect, foldl', (\\))
import Control.Monad.State
import Control.Arrow (second)

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf first second =
  first `intersect` second == first

isLowerOrEqual :: Eq sta => [sta] -> [sta] -> Bool
isLowerOrEqual =
  isSubsetOf

post' :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> [[sta]]
post' fa state = fmap (($ state) . flip (post fa)) (symbols fa)

type InnerState sta = ([[sta]], [[sta]])
type Post sta = [sta] -> [[sta]]
type IsRejecting sta = [sta] -> Bool

isUniversal:: (Eq sym, Eq sta) => Fa sym sta -> Bool
isUniversal fa =
  isMacrostateAccepting fa (initialStates fa) && while'
    where
      while' =
        evalState (while (post' fa) (not . isMacrostateAccepting fa)) ([], [initialStates fa])

moveRFromNextToProcessed :: State (InnerState sta) [sta]
moveRFromNextToProcessed = state $ \(processed, r : next') ->
  (r, (r : processed, next'))

while :: Eq sta => Post sta -> IsRejecting sta -> State (InnerState sta) Bool
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

processPostStates :: Eq sta => [[sta]] -> IsRejecting sta -> State (InnerState sta) Bool
processPostStates [] _ =
  return False
processPostStates (p : ps) isRejecting =
  if isRejecting p
    then return True
    else do
      (processed, next) <- get
      if not $ or [s `isLowerOrEqual` p | s <- processed `union` next]
        then do
          removeFromStateAllGreaterThan p
          addToNext p
          processPostStates ps isRejecting
        else processPostStates ps isRejecting

removeFromStateAllGreaterThan :: Eq sta => [sta] -> State (InnerState sta) ()
removeFromStateAllGreaterThan p = do
  (processed, next) <- get
  state $ \(processed, next) ->
    let
      processed' = processed \\ [s | s <- processed, p `isLowerOrEqual` s ]
      next' = next \\ [s | s <- next, p `isLowerOrEqual` s ]
    in
      ((), (processed', next'))

addToNext :: [sta] -> State (InnerState sta) ()
addToNext p =
  state $ \(processed, next) ->
    ((), (processed, p : next))
