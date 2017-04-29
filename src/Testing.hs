module Testing
  ( unsafeLoadFa
  ) where

import Types.Fa (Fa, Symbol, State)
import Parsing.General (loadFa)
import System.IO.Unsafe (unsafePerformIO)

-- |Returns a FTA parsed from a file specified by its filename. This is an unsafe IO function.
unsafeLoadFa :: FilePath -> Fa Symbol State
unsafeLoadFa =
  getFa . unsafePerformIO . loadFa
    where
      getFa (Left parseError) = error "Parsing error"
      getFa (Right fa) = fa
