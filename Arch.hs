module Arch
  ( nrCycles
  , addrMode, AddrMode(..)
  ) where

import Data.Char (isOctDigit)
import Numeric (readOct)

import Monad
import Types
import Util

nrCycles :: Integral a => Instr -> a
nrCycles i = nrCyclesOp i + nrCyclesMem i

nrCyclesOp :: Integral a => Instr -> a
nrCyclesOp (IOT {})   = 0
nrCyclesOp (OP1 {})   = 1
nrCyclesOp (OP2 {})   = 1
nrCyclesOp (OP3 {})   = 1
nrCyclesOp (UNK {})   = 0
nrCyclesOp (JMP {})   = 1
nrCyclesOp _          = 2 -- Memory Op

nrCyclesMem :: Integral a => Instr -> a
nrCyclesMem i =
  case addrMode i of
    ModeAutoIndexing   -> 2
    ModeIndirect       -> 1
    ModeDirect         -> 0
    NonMemoryOperation -> 0

data AddrMode = ModeIndirect | ModeDirect
              | ModeAutoIndexing | NonMemoryOperation
              deriving (Eq, Ord, Show)

addrMode :: Instr -> AddrMode
addrMode i =
    case typeOf i of
      MemOp -> case indirection i of
                 Indirect | page i == ZeroPage &&
                            offset i >= oct 10 &&
                            offset i <= oct 17 -> ModeAutoIndexing
                          | otherwise          -> ModeIndirect
                 Direct                        -> ModeDirect
      _     -> NonMemoryOperation
