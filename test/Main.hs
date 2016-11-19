module Main where

import qualified Data.ByteString as B
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.Error

import Data.List
import Control.Monad
import Debug.Trace

import Parsing.General
import Types.Fwa as Fwa
import Types.Fta as Fta

main :: IO ()
main = defaultMainWithOpts
  [ testCase "testFwa01" testFwa01
  , testCase "testFwa02" testFwa02
  , testCase "testFwa03" testFwa03
  , testCase "testFta01" testFta01
  , testCase "testFta02" testFta02
  , testCase "testFta03" testFta03
  ]
  mempty

testFwa01 :: Assertion
testFwa01 =
  sequence_ $ fmap (\name -> assertFwa "Cannot parse" name (const True)) testFiles

testFwa02 :: Assertion
testFwa02 =
  assertFwa "Incorrect state count" (head testFiles) (\fwa -> length (Fwa.states fwa) == 4)

testFwa03 :: Assertion
testFwa03 =
  assertFwa "Incorrect transition count" (head testFiles) (\fwa -> length (Fwa.transitions fwa) == 14)

testFta01 :: Assertion
testFta01 =
  sequence_ $ fmap (\name -> assertFta "Cannot parse" name (const True)) testFiles

testFta02 :: Assertion
testFta02 =
  assertFta "Incorrect state count" (head testFiles) (\fta -> length (Fta.states fta) == 4)

testFta03 :: Assertion
testFta03 =
  assertFta "Incorrect transition count" (head testFiles) (\fta -> length (Fta.transitions fta) == 14)

assertFwa :: String -> FilePath -> (Fwa -> Bool) -> Assertion
assertFwa errorMessage filePath condition = loadAndParseFwa filePath >>= \fwa ->
  case fwa of
    Left parseError -> assertFailure parseError
    Right fwa -> unless (condition fwa) (assertFailure errorMessage)

assertFta :: String -> FilePath -> (Fta -> Bool) -> Assertion
assertFta errorMessage filePath condition = loadAndParseFta filePath >>= \fta ->
  case fta of
    Left parseError -> assertFailure parseError
    Right fwa -> unless (condition fwa) (assertFailure errorMessage)

testFiles =
  fmap ("test/TestFiles/" ++) ["0.txt", "1.txt", "2.txt", "3.txt", "4.txt"]
