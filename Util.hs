module Util where

import Numeric (readOct)
import Data.Char

-- Interprete a numeric literal as octal
oct = fst . head . readOct . filter isOctDigit . show :: Int -> Int
