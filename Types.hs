module Types 
  ( -- * Aliases
    Branches, MemoryLog, Register, Memory
    -- * Types
  , MachineState(..)
  , Purpose(..)
  , Stats(..)
    -- * ISR Types
  , Value(..)
  , Instr(..)
  , PDPOp(..)
  , PDPOps
  , IOOp(..)
  , MicroOp1(..)
  , MicroOp2(..)
  , MicroOp3(..)
  , Indirection(..)
  , MemPage(..)
  , Addr(..)
  , Offset
    -- * Helpers
  , initialState, initialMemory) where

import qualified Data.Map as M

type Branches = [(Addr,Addr)]
type MemoryLog = [(Purpose,Addr)]

-- The Stats needing tracked for the assignment
data Stats =
  Stats { nrCycles :: Integer
        , instrBreakdown :: M.Map PDPOp Integer
        }

type Register = Int -- 12 bit contents

-- The registers of a PDP8 are mostly
-- 12 bit but there are two odd balls (ir,lb)
data MachineState =
  MS { pc,sr                 :: Register
     , lb   :: Int -- 1 bit
     , mem  :: Memory
     } deriving (Eq, Ord, Show)

-- The machine memory associates addresses to integers
type Memory = M.Map Addr Int

data Purpose = DataRead | DataWrite | InstrFetch
  deriving (Eq, Ord, Show, Enum)

initialState  = MS 0 0 0 initialMemory
initialMemory = M.empty

data Value = VInstr { vInstr :: Instr } | VAddr { vAddr :: Addr }
  deriving (Eq, Ord, Show)

type Offset = Int

-- The 'Instr'uncation data type carries the decoded op,
-- indirection, memory page flags, and offset if applicable,
-- and the raw integer that was decoded.
data Instr = Instr 
  { instrOp          :: PDPOp
  , instrIndirection :: (Maybe Indirection)
  , instrMemPage     :: (Maybe MemPage)
  , instrOffset      :: (Maybe Offset)
  , instrCode        :: Int }
  deriving (Eq, Ord, Show)

type PDPOps = [PDPOp]
data PDPOp = PDPOpMem MemOp
           | PDPOpIO IOOp 
           | PDPOpMicro1 [MicroOp1]
           | PDPOpMicro2 [MicroOp2]
           | PDPOpMicro3 [MicroOp3]
           | UnknownOp
  deriving (Eq, Ord, Show)

data MemOp
  = AND  -- Logical and
  | TAD  -- two's comp add
  | ISZ  -- Increment memory and inc PC if zero
  | DCA  -- store acc to memory and clear acc
  | JMS  -- store PC to addr, read new pc from addr + 1
  | JMP  -- read new pc from addr
  deriving (Eq, Ord, Show, Enum)

data IOOp =
  -- A general placeholder for IO
    IOOp
  deriving (Eq, Ord, Show, Enum)

data MicroOp1 =
  -- Microcodes
    NOP
  | CLA1
  | CLL
  | CMA
  | CML
  | IAC
  | RAR
  | RTR
  | RAL
  | RTL
  deriving (Eq, Ord, Show, Enum)

data MicroOp2 =
  -- Group 2
    SMA
  | SZA
  | SNL
  | SPA
  | SNA
  | SZL
  | SKP
  | CLA2
  | OSR
  | HLT
  deriving (Eq, Ord, Show, Enum)

data MicroOp3 =
  -- Group 3
    CLA3
  | MQL
  | MQA
  | SWP
  | CAM
  deriving (Eq, Ord, Show, Enum)

data Indirection = Direct | Indirect
  deriving (Eq, Ord, Show)

data MemPage = ZeroPage | CurrentPage
  deriving (Eq, Ord, Show)

newtype Addr = Addr { unAddr :: Int }
  deriving (Eq, Ord, Show)