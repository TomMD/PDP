module Util where

import Numeric (readOct)
import Data.Char
import Data.Bits

show4 :: (Show a, Integral a) => a -> String
show4 = reverse . take 4 . reverse . ("0000" ++) . show

-- Interprete a numeric literal as octal
oct :: Integral a => Int -> a
oct = fst . head . readOct . filter isOctDigit . show

-- "To Binary" intented to appear like
-- 0b11011 in the same way 0x.... is hex
ob :: (Bits a, Integral a) => Integer -> a
ob = foldl (\s x -> shiftL s 1 .|. x) 0
   . map (fromIntegral . digitToInt)
   . map (\x -> if x `notElem` "01"
                then error "Invalid 'binary' used in ob"
                else x)
   . show
