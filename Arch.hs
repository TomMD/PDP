module Arch
  ( nrCycles
  , addrMode, AddrMode(..)
    -- Helpers
  , addrModeIndirect, addrModeAutoIndexing, addrModeDirect
  ) where

import Data.Char (isOctDigit)
import Numeric (readOct)

import Monad
import Types
import Util

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
addrModeIndirect = (== ModeIndirect) . addrMode

addrModeAutoIndexing :: Instr -> Bool
addrModeAutoIndexing = (== ModeAutoIndexing) . addrMode

addrModeDirect :: Instr -> Bool
addrModeDirect = (== ModeDirect) . addrMode

data AddrMode = ModeIndirect | ModeDirect | ModeAutoIndexing | NonMemoryOperation
              deriving (Eq, Ord, Show)

addrMode :: Instr -> AddrMode
addrMode (Instr op (Just Indirect) (Just m) (Just o) _)
  | o >= oct 10 && o <= oct 17 && m == ZeroPage = ModeAutoIndexing
  | otherwise = ModeIndirect
addrMode (Instr op (Just Direct) _ _ _) = ModeDirect
addrMode _ = NonMemoryOperation