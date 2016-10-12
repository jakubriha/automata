module Main where

import qualified Data.ByteString as B
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec.Error
import Text.Parsec.Pos (sourceName, sourceLine, sourceColumn)
import Data.List
import Control.Monad

import Parsing.General
import Types.Fta

main :: IO ()
main = defaultMainWithOpts
       [ testCase "testFta01" testFta01
       , testCase "testFta02" testFta02
       , testCase "testFta03" testFta03
       ]
       mempty

testFta01 :: Assertion
testFta01 = sequence_ $ fmap (\name -> assertFta "Cannot parse" name (const True)) testFiles

testFta02 :: Assertion
testFta02 = assertFta "Incorrect state count" (head testFiles) (\fta -> length (states fta) == 4)

testFta03 :: Assertion
testFta03 = assertFta "Incorrect transition count" (head testFiles) (\fta -> length (transitions fta) == 14)

assertFta :: String -> FilePath -> (Fta -> Bool) -> Assertion
assertFta errorMessage filePath condition = loadAndParseFta filePath >>= \fta ->
               case fta of
                   Left parseError -> assertFailure $ errorToString parseError
                   Right state     -> unless (condition state) (assertFailure errorMessage)

testFiles = fmap ("test/TestFiles/" ++) ["0.txt", "1.txt", "2.txt", "3.txt", "4.txt"]

errorToString :: ParseError -> String
errorToString parseError = (intercalate ":" . fmap ($ parseError)) [getName, getLine, getColumn, getMessage]
    where getMessage = concatMap messageString . errorMessages
          getName = show . sourceName . errorPos
          getLine    = show . sourceLine . errorPos
          getColumn  = show . sourceColumn . errorPos

