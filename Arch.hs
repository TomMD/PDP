module Arch
  ( nrCycles
  ) where

import Numeric (readOct)
import Data.Char (isOctDigit)
import Memory

-- FIXME put any constants here!  Constants such as the auto-increment values.

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