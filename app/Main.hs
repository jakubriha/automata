module Main where

import Parsing.General (parseFa)
import Types.Fa (Fa)
import qualified Data.ByteString as B
import Operations.Regular (union, intersect)
import Operations.Antichain.Universality as Universality
import Operations.Antichain.Inclusion as Inclusion
import System.Environment (getArgs)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Time (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import System.IO (stderr, hPutStrLn)
import Criterion.Main

main :: IO ()
main = do
  arguments <- getArgs
  case parseArguments arguments of
    Nothing -> putStrLn "Wrong arguments"
    Just (filePath1, filePath2) -> do
      file1 <- B.readFile filePath1
      file2 <- B.readFile filePath2
      case parseFa file1 of
        Left error -> fail error
        Right fa1 ->
          case parseFa file2 of
            Left error -> fail error
            Right fa2 ->
              print $ intersect (force fa1) (force fa2)
              -- defaultMain [ bench "universality" $ nf (intersect (force fa1)) (force fa2) ]

parseArguments :: [String] -> Maybe (FilePath, FilePath)
parseArguments [_, x, y] =
  Just (x, y)
parseArguments _ =
  Nothing
