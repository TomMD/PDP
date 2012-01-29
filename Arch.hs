module Arch
  ( nrCycles
  ) where

import Numeric (readOct)
import Data.Char (isOctDigit)
import Memory
import Types

-- FIXME put any constants here!  Constants such as the auto-increment values.

nrCyclesOp :: Integral a => PDPOp -> a
nrCyclesOp o
  | opIsMem o          = 2
  | opIsIO o           = 0
  | opIsMicro o        = 1
  | otherwise          = 0 --Unknown ops

nrCycles :: Integral a => Instr -> a
nrCycles instr@(Instr op _ _ _ _)
  | addrModeAutoIndexing instr = 2 + nrCyclesOp op
  | addrModeIndirect instr     = 1 + nrCyclesOp op
  | otherwise                  =     nrCyclesOp op

addrModeIndirect :: Instr -> Bool
addrModeIndirect = undefined

addrModeAutoIndexing :: Instr -> Bool
addrModeAutoIndexing = undefined