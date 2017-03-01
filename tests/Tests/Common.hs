module Tests.Common
  ( assertFa
  , assert2Fa
  ) where

import Test.Hspec

import Types.Fa (Fa, State, Symbol)
import Parsing.General (loadFa)

assertFa :: FilePath -> (Fa Symbol State -> Bool) -> Expectation
assertFa filePath condition =
  loadFa ("tests/Examples/" ++ filePath) >>= \fa ->
    case fa of
      Left parseError -> expectationFailure parseError
      Right fa -> fa `shouldSatisfy` condition

assert2Fa :: FilePath -> FilePath -> (Fa Symbol State -> Fa Symbol State -> Bool) -> Expectation
assert2Fa filePath1 filePath2 condition =
  do
    firstFa <- loadFa ("tests/Examples/" ++ filePath1)
    secondFa <- loadFa ("tests/Examples/" ++ filePath2)
    case firstFa of
      Left parseError -> expectationFailure parseError
      Right fa1 -> case secondFa of
                     Left parseError -> expectationFailure parseError
                     Right fa2 -> fa2 `shouldSatisfy` condition fa1

