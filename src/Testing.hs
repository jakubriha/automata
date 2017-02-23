module Testing
  ( testFa
  , test2Fa
  ) where

import Types.Fa
import Parsing.General (loadFa)

testFa :: (Eq sym, Show sym, Eq sta, Show sta) => FilePath -> (Fa Symbol State -> Fa sym sta) -> IO ()
testFa filePath operation =
  loadFa filePath >>= executeOperation
    where
      executeOperation (Left parseError) = putStrLn parseError
      executeOperation (Right fa) = print (operation fa) 

test2Fa :: (Eq sym, Show sym, Eq sta, Show sta) => FilePath -> FilePath -> (Fa Symbol State -> Fa Symbol State -> Fa sym sta) -> IO ()
test2Fa filePath1 filePath2 operation =
  do firstFa <- loadFa filePath1
     secondFa <- loadFa filePath2
     case firstFa of
       Left parseError -> putStrLn parseError
       Right fa1 -> case secondFa of
                  Left parseError -> putStrLn parseError
                  Right fa2 -> print (operation fa1 fa2)

