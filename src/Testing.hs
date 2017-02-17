module Testing
  ( test2Fa
  ) where

import Types.Fa
import Parsing.General (loadFa)

test2Fa :: (Eq i, Show i, Eq s, Show s) => FilePath -> FilePath -> (Fa Label State -> Fa Label State -> Fa i s) -> IO ()
test2Fa filePath1 filePath2 operation =
  do firstFa <- loadFa filePath1
     secondFa <- loadFa filePath2
     case firstFa of
       Left parseError -> putStrLn parseError
       Right fa1 -> case secondFa of
                  Left parseError -> putStrLn parseError
                  Right fa2 -> print (operation fa1 fa2)

