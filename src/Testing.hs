module Testing
  ( test2Fwa
  ) where

import Types.Fwa
import Parsing.General (loadFwa)

test2Fwa :: FilePath -> FilePath -> (Fwa -> Fwa -> Fwa) -> IO ()
test2Fwa filePath1 filePath2 operation =
  do firstFwa <- loadFwa filePath1
     secondFwa <- loadFwa filePath2
     case firstFwa of
       Left parseError -> putStrLn parseError
       Right fwa1 -> case secondFwa of
                  Left parseError -> putStrLn parseError
                  Right fwa2 -> print (operation fwa1 fwa2)

