module Arch
  ( nrCycles
  ) where

import Numeric (readOct)
import Data.Char (isOctDigit)

nrCyclesOp :: PDPOp -> Int
nrCyclesOp o
  | op >= 0 && <= 4 = 2
  | op == 5         = 1
  | op == 6         = 0
  | otherwise       = 1
 where
 op = fromEnum o

nrCycles :: Instr -> Int
nrCycles instr@(Instr op _ _ _ _)
  | addrModeAutoIndexing instr = 2 + nrCyclesOp op
  | addrModeIndirect instr     = 1 + nrCyclesOp op
  | otherwise                  =     nrCyclesOp op

addrModeAutoIndexing (Instr op ind mp off _) =
  ind == Just Indirect &&
  mp  == Just ZeroPage &&
  off >= Just (oct 10)
  off <= Just (oct 17)
addrModeIndirect (Instr op ind mp off _) =
  ind == Just Indirect && (   mp /= Just ZeroPage
                           || off < Just (oct 10)
                           || off > Just (oct 17)
                          )

-- Interprete a numeric literal as octal
oct = fst . head . readOct . filter isOctDigit . show :: Int -> Int
