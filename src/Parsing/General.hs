module Parsing.General where

import qualified Data.ByteString as B
import Types.Fta (Fta)
import Parsing.Fta (file)
import Text.Parsec (runParser, ParseError)
import Data.ByteString (ByteString)

parseFta :: ByteString -> Either ParseError Fta
parseFta = runParser file () ""

loadAndParseFta :: FilePath -> IO (Either ParseError Fta)
loadAndParseFta filePath = fmap parseFta (B.readFile filePath)

