module Operations.Regular
  ( charsToSymbols
  , isMacrostateAccepting
  , run
  , module Operations.WithExternalSymbols
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
import Operations.WithExternalSymbols (post)
import qualified Operations.WithExternalSymbols as ExternalSymbols

-- |Converts 'String' to a list of symbols.
charsToSymbols :: String -> [Symbol]
charsToSymbols = fmap (: [])

-- |Determines whether a macro-state is accepting.
isMacrostateAccepting :: Eq sta => Fa sym sta -> [sta] -> Bool
isMacrostateAccepting fa states =
  states `List.intersect` finalStates fa /= []

-- |Returns the post states for each symbol of the alphabet.
postForEachSymbol :: (Eq sym, Eq sta) => Fa sym sta -> [sta] -> [[sta]]
postForEachSymbol fa =
  ExternalSymbols.postForEachSymbol (symbols fa) fa

-- |Checks whether a FA accepts a string.
run :: (Eq sym, Eq sta) => Fa sym sta -> [sym] -> Bool
run fa =
  run' (initialStates fa)
    where
      run' currentStates [] =
        isMacrostateAccepting fa currentStates
      run' currentStates (x:xs) =
        run' (post fa currentStates x) xs

-- |Creates a union of two FAs.
union :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `List.union` initialStates2)
    (finalStates1 `List.union` finalStates2)
    (transitions1 `List.union` transitions2)

-- |Creates an intersection of two FAs.
intersect :: Eq sym => Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect fa1 fa2 =
  ExternalSymbols.intersect (symbols fa1 `List.union` symbols fa2) fa1 fa2

-- |Converts a FA to an equivalent deterministic FA.
determinize :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
determinize fa =
  ExternalSymbols.determinize (symbols fa) fa

-- |Creates a complement of a FA.
complement :: (Eq sym, Eq sta) => Fa sym sta -> Fa sym [sta]
complement fa =
  ExternalSymbols.complement (symbols fa) fa

-- |Checks whether a FA accepts an empty language.
isEmpty :: (Eq sym, Ord sta) => Fa sym sta -> Bool
isEmpty fa =
  ExternalSymbols.isEmpty (symbols fa) fa

-- |Checks whether the first FA is subset of the second FA using the naive algorithm.
isSubsetOf :: (Eq sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1 fa2 =
  ExternalSymbols.isSubsetOf (symbols fa1) (symbols fa2) fa1 fa2  

-- |Checks whether a FA accepts all possible strings using the naive algorithm.
isUniversal :: (Eq sym, Ord sta) => Fa sym sta -> Bool
isUniversal fa =
    ExternalSymbols.isUniversal (symbols fa) fa
