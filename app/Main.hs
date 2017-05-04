module Main where

import Parsing.General (parseFa)
import Types.Fa (Fa)
import qualified Data.ByteString as B
import Operations.Regular as Regular
import Operations.Antichain.Universality as Universality
import System.Environment (getArgs)
import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate)
import Data.Time (diffUTCTime, getCurrentTime)
import Numeric (showFFloat)
import System.IO (stderr, hPutStrLn)

main :: IO ()
main = do
  arguments <- getArgs
  case parseArguments arguments of
    Nothing -> putStrLn "Wrong arguments"
    Just filePath -> do
      file <- B.readFile filePath
      case parseFa file of
        Left error -> fail error
        Right fa ->
          benchmarkForce Universality.isUniversal (force fa) >>= print

parseArguments :: [String] -> Maybe FilePath
parseArguments [x] =
  Just x
parseArguments _ =
  Nothing

benchmarkForce :: (NFData a, NFData p) => (p -> a) -> p -> IO a
benchmarkForce action param = do
  let diffTimeInSeconds t1 t2 = realToFrac (t2 `diffUTCTime` t1) :: Double
  paramEval <- (evaluate . force) param
  before <- getCurrentTime
  result <- (evaluate . force) (action paramEval)
  after <- getCurrentTime 
 
  hPutStrLn stderr $ showFFloat Nothing (diffTimeInSeconds before after) "" 
 
  return result 



-- main :: IO ()
-- main = do
--   arguments <- getArgs
--   case parseArguments arguments of
--     Nothing -> putStrLn "Wrong arguments"
--     Just (filePath1, filePath2) -> do
--       file1 <- B.readFile filePath1
--       file2 <- B.readFile filePath2
--       case parseFa file1 of
--         Left error -> fail error
--         Right fa1 ->
--           case parseFa file2 of
--             Left error -> fail error
--             Right fa2 ->
--               return ()
              
--               -- print $ intersect (force fa1) (force fa2)
--               -- defaultMain [ bench "universality" $ nf (intersect (force fa1)) (force fa2) ]

-- parseArguments :: [String] -> Maybe (FilePath, FilePath)
-- parseArguments [_, x, y] =
--   Just (x, y)
-- parseArguments _ =
--   Nothing
