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
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Set (Set, empty, toList, fromList, unions, intersection)
import Data.List ((\\), nub)
import qualified Operations.WithExternalSymbols as ExternalSymbols

charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

isMacrostateAccepting :: Eq sta => Fa sym sta -> [sta] -> Bool
isMacrostateAccepting fa states =
  states `List.intersect` finalStates fa /= []

run :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> Bool
run fa =
  run' (initialStates fa)
    where
      run' currentStates [] =
        isMacrostateAccepting fa currentStates
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs

post :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> sym -> [sta]
post fa currentStates symbol =
  fmap finalState $ filter isApplicableTransition $ transitions fa
    where
      isApplicableTransition (Transition tSymbol state _) =
        tSymbol == symbol && state `elem` currentStates

postForEachSymbol :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> [[sta]]
postForEachSymbol fa =
  ExternalSymbols.postForEachSymbol (symbols fa) fa

union :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `List.union` initialStates2)
    (finalStates1 `List.union` finalStates2)
    (transitions1 `List.union` transitions2)

transitionsCreator
  :: (Eq sym1, Eq sym2)
  => (sym1 -> sym2 -> sym)
  -> (sym1 -> sym2 -> Bool)
  -> Fa sym1 sta1
  -> Fa sym2 sta2
  -> [Transition sym (sta1, sta2)]
transitionsCreator function predicate fa1 fa2 =
  ExternalSymbols.transitionsCreator (symbols fa1) (symbols fa2) function predicate fa1 fa2

intersect :: Eq sym => Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect fa1 fa2 =
  ExternalSymbols.intersect (symbols fa1) (symbols fa2) fa1 fa2

determinize :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
determinize fa =
  ExternalSymbols.determinize (symbols fa) fa

complement :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
complement fa =
  ExternalSymbols.complement (symbols fa) fa

isEmpty :: (Eq sym, Ord sta) => Fa sym sta -> Bool
isEmpty fa =
  ExternalSymbols.isEmpty (symbols fa) fa

isSubsetOf :: (Eq sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1 fa2 =
  ExternalSymbols.isSubsetOf (symbols fa1) (symbols fa2) fa1 fa2  

isUniversal :: (Eq sym, Ord sta) => Fa sym sta -> Bool
isUniversal fa =
    ExternalSymbols.isUniversal (symbols fa) fa
