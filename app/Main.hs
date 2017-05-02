module Main where

import Parsing.General (parseFa)
import Types.Fa (Fa)
import qualified Data.ByteString as B
import qualified Operations.Antichain.Inclusion as Inclusion
import qualified Operations.Antichain.Universality as Universality
import qualified Operations.Regular as Regular
import System.Environment (getArgs)
import Control.DeepSeq
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
          benchmarkForce Universality.isUniversal fa >>= print
          
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

  -- Force the first time to measure computation + forcing
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
--                 benchmarkForce Regular.intersect fa1 fa2 >>= print

-- parseArguments :: [String] -> Maybe (FilePath, FilePath)
-- parseArguments (x:y:_) =
--   Just (x, y)
-- parseArguments _ =
--   Nothing

-- http://stackoverflow.com/questions/18612303/haskell-how-to-benchmark-a-computation-accurately-with-deepseq-force

-- benchmarkForce :: (NFData a, NFData p, NFData r) => (p -> r -> a) -> p -> r -> IO a
-- benchmarkForce action param1 param2 = do
--   let diffTimeInSeconds t1 t2 = realToFrac (t2 `diffUTCTime` t1) :: Double

--   param1Eval <- (evaluate . force) param1
--   param2Eval <- (evaluate . force) param2

--   before <- getCurrentTime

--   -- Force the first time to measure computation + forcing
--   result <- (evaluate . force) (action param1Eval param2Eval)

--   after <- getCurrentTime

--   hPutStrLn stderr $ showFFloat Nothing (diffTimeInSeconds before after) ""

--   return result
