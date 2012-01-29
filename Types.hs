module Types 
  ( -- * Aliases
    Branches, MemoryLog, Int12, Memory
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
  , initialState, initialMemory, initialStats
  , opIsMem, opIsIO, opIsMicro) where

import qualified Data.Map as M
import Data.Bits
import Data.Char (digitToInt)
import Numeric
import Util

-- |Branches are recorded as (address of the branching instruction,
-- target address)
type Branches = [(Addr,Addr)]
type MemoryLog = [(Purpose,Addr)]

-- The Stats needing tracked for the assignment
data Stats =
  Stats { cycleCnt :: Integer
        , instrBreakdown :: M.Map PDPOp Integer
        }

initialStats = Stats 0 M.empty

-- The registers of a PDP8 are mostly
-- 12 bit but there are two odd balls (ir,lb)
data MachineState =
  MS { pc,sr,ac     :: Int12
     , lb           :: Int -- 1 bit
     , mem          :: Memory
     } deriving (Eq, Ord, Show)

-- The machine memory associates addresses to integers
type Memory = M.Map Addr Int12

data Purpose = DataRead | DataWrite | InstrFetch
  deriving (Eq, Ord, Show, Enum)

initialState  = MS 0 0 0 0 initialMemory
initialMemory = M.empty

data Value = VInstr { vInstr :: Instr } | VAddr { vAddr :: Addr }
  deriving (Eq, Ord, Show)

type Offset = Int12

-- |The 'Instr'uncation data type carries the decoded op, indirection,
-- memory page flags, and offset if applicable, and the raw integer
-- that was decoded.
data Instr = Instr 
  { instrOp          :: PDPOp
  , instrIndirection :: (Maybe Indirection)
  , instrMemPage     :: (Maybe MemPage)
  , instrOffset      :: (Maybe Offset)
  , instrCode        :: Int12 }
  deriving (Eq, Ord, Show)

type PDPOps = [PDPOp]
data PDPOp = PDPOpMem MemOp
           | PDPOpIO IOOp 
           | PDPOpMicro1 [MicroOp1]
           | PDPOpMicro2 [MicroOp2]
           | PDPOpMicro3 [MicroOp3]
           | UnknownOp
  deriving (Eq, Ord, Show)

opIsMem :: PDPOp -> Bool
opIsMem (PDPOpMem _) = True
opIsMem _            = False

opIsIO :: PDPOp -> Bool
opIsIO (PDPOpIO _) = True
opIsIO _           = False

opIsMicro :: PDPOp -> Bool
opIsMicro (PDPOpMicro1 _) = True
opIsMicro (PDPOpMicro2 _) = True
opIsMicro (PDPOpMicro3 _) = True
opIsMicro _               = False

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

newtype Addr = Addr { unAddr :: Int12 }
  deriving (Eq, Ord, Show)

-- The PDP8 operates on 12-bit integers.  To model this we have an
-- "Int12" type that is guarenteed to remain within the range -2^11
-- .. 2^11-1 It's a bit verbose and not entirely necessary, but the
-- certainty that we won't some-how get 2^32 when reading from our
-- model PDP8 memory is nice to have.
newtype Int12 = Int12 { unInt12 :: Int }
  deriving (Eq, Ord)

-- Read and show instances of Int12 operate in octal.
instance Show Int12 where
  show i
   | i < 0     = showOct (2^12 + (unInt12 i)) ""
   | otherwise = showOct (unInt12 i) ""

instance Read Int12 where
  readsPrec _ o = [(fromIntegral i,s) | (i,s) <- readOct o]

modOp12 op (Int12 a) (Int12 b) =
  let res = a `op` b
  in if res < (-2^11)
      then Int12 (res + 2^12)
      else if res > (2^11 - 1)
              then Int12 (res - 2^12)
              else Int12 res

instance Bounded Int12 where
  minBound = -2^11
  maxBound = 2^11 - 1

instance Bits Int12 where
  (.|.) = modOp12 (.|.)
  (.&.) = modOp12 (.&.)
  xor   = modOp12 xor
  complement = (+0) . Int12 . complement . unInt12
  shift (Int12 i) r = Int12 (shift i r) + 0
  rotate (Int12 i) r = Int12 (rotate i r) + 0
  bit 11 = minBound
  bit i | i > 11 = 0
        | otherwise = Int12 (bit i)
  setBit i r = i .|. bit r
  clearBit i r = i .&. complement (bit r)
  bitSize  _   = 12
  isSigned _   = True

instance Num Int12 where
  (+) = modOp12 (+)
  (*) = modOp12 (*)
  (-) = modOp12 (-)
  abs = Int12 . abs . unInt12
  fromInteger i
     | i < (-2^11) = fromInteger (i + 2^12)
     | i > (2^11-1) = fromInteger (i - 2^12)
     | otherwise = Int12 (fromIntegral i)
  signum = fromIntegral . signum . unInt12

instance Integral Int12 where
  quot = modOp12 quot
  rem = modOp12 rem
  div = modOp12 div
  toInteger (Int12 i) = toInteger i
  quotRem a b = (quot a b, rem a b)

instance Real Int12 where
  toRational (Int12 i) = toRational i

instance Enum Int12 where
  toEnum   = fromIntegral
  fromEnum = fromIntegral