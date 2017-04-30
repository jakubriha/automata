{-# LANGUAGE MonadComprehensions #-}

module Operations.Product
  ( union
  ) where

import Types.Fa hiding (State, state)
import Operations.WithExternalSymbols (post, isMacrostateAccepting)
import Data.Set.Monad (Set)
import qualified Data.Set.Monad as Set
import Control.Monad.State
import Control.Monad.Loops (whileM_)

type MacroState sta1 sta2 = (Set sta1, Set sta2)
type Front sta1 sta2 = Set (MacroState sta1 sta2)
type Processed sta1 sta2 = Set (MacroState sta1 sta2)
type InnerState sym sta1 sta2 = (Front sta1 sta2, Processed sta1 sta2, Set (Transition sym (MacroState sta1 sta2)), Fa sym sta1, Fa sym sta2)

-- |Creates a union of two FAs with product state ('sta1', 'sta2').
union :: (Ord sym, Ord sta1, Ord sta2) => Fa sym sta1 -> Fa sym sta2 -> Fa sym (Set sta1, Set sta2)
union fa1 fa2 =
  evalState while (Set.singleton (initialStates fa1, initialStates fa2), Set.empty, Set.empty, fa1, fa2)

while :: (Ord sym, Ord sta1, Ord sta2) => State (InnerState sym sta1 sta2) (Fa sym (Set sta1, Set sta2))
while = do
  whileM_ frontNotEmpty whileBody
  (_, processed, transitions, fa1, fa2) <- get
  return $ createUnionFa fa1 fa2 processed transitions

frontNotEmpty :: State (InnerState sym sta1 sta2) Bool
frontNotEmpty = state $ \oldState@(front, _, _, _, _) ->
  (not $ null front, oldState)

whileBody :: (Ord sym, Ord sta1, Ord sta2) => State (InnerState sym sta1 sta2) ()
whileBody =
  pickStateFromFront >>= createTransitionForEachSymbolFromState

pickStateFromFront :: (Ord sta1, Ord sta2) => State (InnerState sym sta1 sta2) (MacroState sta1 sta2)
pickStateFromFront = state $ \(front, processed, transitions, fa1, fa2) ->
  let
    r = Set.findMin front
    front' = Set.delete r front
  in
    (r, (front', processed, transitions, fa1, fa2))

createTransitionForEachSymbolFromState :: (Ord sym, Ord sta1, Ord sta2) => MacroState sta1 sta2 -> State (InnerState sym sta1 sta2) ()
createTransitionForEachSymbolFromState macroState@(state1, state2) = state $ \oldState@(front, processed, transitions, fa1, fa2) ->
  let
    alphabet =
      symbols fa1 `Set.union` symbols fa2
    newState symbol =
      (post fa1 state1 symbol, post fa2 state2 symbol)
    newTransitions =
      [ Transition symbol macroState (newState symbol)
      | symbol <- alphabet
      ]
  in
    if macroState `Set.member` processed
      then ((), oldState)
      else ((), (fmap newState alphabet `Set.union` front, macroState `Set.insert` processed, newTransitions `Set.union` transitions, fa1, fa2))

createUnionFa :: (Ord sta1, Ord sta2)
  => Fa sym sta1
  -> Fa sym sta2
  -> Processed sta1 sta2
  -> Set (Transition sym (MacroState sta1 sta2))
  -> Fa sym (Set sta1, Set sta2)
createUnionFa fa1 fa2 states =
  Fa (Set.singleton (initialStates fa1, initialStates fa2)) finalStates
    where
      finalStates =
        [ finalState
        | finalState <- states
        , isMacrostateAccepting fa1 (fst finalState) || isMacrostateAccepting fa2 (snd finalState)
        ] 
