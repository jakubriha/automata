module Operations.Regular
  ( charsToSymbols
  , isMacrostateAccepting
  , run
  , post
  , postForEachSymbol
  , union
  , intersect
  , determinize
  , complement
  , isEmpty
  , isSubsetOf
  , isUniversal
  ) where

import Types.Fa
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Data.List ((\\), nub)
import qualified Operations.WithExternalSymbols as ExternalSymbols

charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

isMacrostateAccepting :: Ord sta => Fa sym sta -> Set sta -> Bool 
isMacrostateAccepting fa states = 
  not $ Set.null $ states `Set.intersection` finalStates fa

run :: (Ord sym, Ord sta) => Fa sym sta -> [sym] -> Bool
run fa =
  run' (initialStates fa)
    where
      run' currentStates [] =
        isMacrostateAccepting fa currentStates
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs

post :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> sym -> Set sta
post fa currentStates symbol =
  fmap finalState $ Set.filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

postForEachSymbol :: (Ord sym, Ord sta) => Fa sym sta -> Set sta -> Set (Set sta)
postForEachSymbol fa = 
  ExternalSymbols.postForEachSymbol (symbols fa) fa

union :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `Set.union` initialStates2)
    (finalStates1 `Set.union` finalStates2)
    (transitions1 `Set.union` transitions2)

transitionsCreator
  :: (Ord sym1, Ord sym2)
  => (sym1 -> sym2 -> sym)
  -> (sym1 -> sym2 -> Bool)
  -> Fa sym1 sta1
  -> Fa sym2 sta2
  -> Set (Transition sym (sta1, sta2))
transitionsCreator function predicate fa1 fa2 =
  ExternalSymbols.transitionsCreator (symbols fa1) (symbols fa2) function predicate fa1 fa2

intersect :: Ord sym => Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect fa1 fa2 =
  ExternalSymbols.intersect (symbols fa1) (symbols fa2) fa1 fa2

determinize :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym (Set sta)
determinize fa =
  ExternalSymbols.determinize (symbols fa) fa

complement :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym (Set sta)
complement fa =
  ExternalSymbols.complement (symbols fa) fa

isEmpty :: (Ord sym, Ord sta) => Fa sym sta -> Bool
isEmpty fa =
  ExternalSymbols.isEmpty (symbols fa) fa

isSubsetOf :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1 fa2 =
  ExternalSymbols.isSubsetOf (symbols fa1) (symbols fa2) fa1 fa2  

isUniversal :: (Ord sym, Ord sta) => Fa sym sta -> Bool
isUniversal fa =
  ExternalSymbols.isUniversal (symbols fa) fa
