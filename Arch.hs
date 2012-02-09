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

nrCyclesOp :: Integral a => InstructionType -> a
nrCyclesOp MemOp     = 2
nrCyclesOp IOOp      = 0
nrCyclesOp UnknownOp = 0
nrCyclesOp _         = 1 -- micro ops

nrCycles :: Integral a => Instr -> a
nrCycles i =
    case typeOf i of
      MemOp | addrModeAutoIndexing i -> 2 + nrCyclesOp MemOp
            | addrModeIndirect i     -> 1 + nrCyclesOp MemOp
      t                              -> nrCyclesOp t

addrModeIndirect :: Instr -> Bool
addrModeIndirect = (== ModeIndirect) . addrMode

addrModeAutoIndexing :: Instr -> Bool
addrModeAutoIndexing = (== ModeAutoIndexing) . addrMode

addrModeDirect :: Instr -> Bool
addrModeDirect = (== ModeDirect) . addrMode

data AddrMode = ModeIndirect | ModeDirect | ModeAutoIndexing | NonMemoryOperation
              deriving (Eq, Ord, Show)

addrMode :: Instr -> AddrMode
addrMode i =
    case typeOf i of
      MemOp -> case indirection i of
                 Indirect | page i == ZeroPage && offset i >= oct 10 && offset i <= oct 17 -> ModeAutoIndexing
                          | otherwise                                                      -> ModeIndirect
                 Direct                                                                    -> ModeDirect
      _     -> NonMemoryOperation
