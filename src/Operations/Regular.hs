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
  , union
  , productUnion
  , intersect
  , determinize
  , complement
  , isEmpty
  , isSubsetOf
  , isUniversal
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

-- |Creates a union of two FAs.
union :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Fa sym sta
union (Fa initialStates1 finalStates1 transitions1) (Fa initialStates2 finalStates2 transitions2) =
  Fa
    (initialStates1 `Set.union` initialStates2)
    (finalStates1 `Set.union` finalStates2)
    (transitions1 `Set.union` transitions2)

-- |Creates a union of two FAs with product state ('sta1', 'sta2').
productUnion :: (Ord sym, Ord sta1, Ord sta2) => Fa sym sta1 -> Fa sym sta2 -> Fa sym (Set sta1, Set sta2)
productUnion fa1 fa2 =
  ExternalSymbols.productUnion allSymbols completeFa1 completeFa2
    where
      allSymbols = symbols fa1 `Set.union` symbols fa2
      completeFa1 = ExternalSymbols.complete allSymbols fa1
      completeFa2 = ExternalSymbols.complete allSymbols fa2

-- |Creates an intersection of two FAs.
intersect :: (Ord sym, Ord sta1, Ord sta2) => Fa sym sta1 -> Fa sym sta2 -> Fa sym (sta1, sta2)
intersect fa1 fa2 =
  ExternalSymbols.intersect (symbols fa1 `Set.union` symbols fa2) fa1 fa2

-- |Converts a FA to an equivalent deterministic FA.
determinize :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym (Set sta) 
determinize fa = 
  ExternalSymbols.determinize (symbols fa) fa

-- |Creates a complement of a FA.
complement :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym (Set sta)
complement fa =
  ExternalSymbols.complement (symbols fa) fa

-- |Checks whether a FA accepts an empty language.
isEmpty :: (Ord sym, Ord sta) => Fa sym sta -> Bool
isEmpty fa =
  ExternalSymbols.isEmpty (symbols fa) fa

-- |Checks whether the first FA is subset of the second FA using the naive algorithm.
isSubsetOf :: (Ord sym, Ord sta) => Fa sym sta -> Fa sym sta -> Bool
isSubsetOf fa1 fa2 =
  ExternalSymbols.isSubsetOf (symbols fa1) (symbols fa2) fa1 fa2  

-- |Checks whether a FA accepts all possible strings using the naive algorithm.
isUniversal :: (Ord sym, Ord sta) => Fa sym sta -> Bool
isUniversal fa =
  ExternalSymbols.isUniversal (symbols fa) fa
