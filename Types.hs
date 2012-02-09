module Types
  ( -- * Aliases
    BranchLog, MemoryLog, Int12, Memory
    -- * Types
  , MachineState(..)
  , Purpose(..)
  , Stats(..)
    -- * ISR Types
  , Value(..)
  , Instr(..)
  , InstructionType(..)
  , MicroOp1(..)
  , LogicalOp(..)
  , MicroOp2(..)
  , SkipOp(..)
  , MicroOp3(..)
  , IOOp(..)
  , Indirection(..)
  , MemPage(..)
  , Addr(..)
  , Offset
    -- * Helpers
  , initialState, initialMemory, initialStats
  , typeOf ) where

import qualified Data.Map as M
import Data.Bits
import Data.Char (digitToInt)
import Numeric
import Util

-- |Branches are recorded as (address of the branching instruction,
-- target address)
type BranchLog = [(Addr,Addr)]
type MemoryLog = [(Purpose,Addr)]

-- The Stats needing tracked for the assignment
data Stats =
  Stats { cycleCnt :: Integer
        , instrBreakdown :: M.Map InstructionType Integer
        , branchLog :: BranchLog
        , memoryLog :: MemoryLog
        }

initialStats = Stats 0 M.empty [] []

-- The registers of a PDP8 are mostly
-- 12 bit but there are two odd balls (ir,lb)
data MachineState =
  MS { pc,sr,ac,ir  :: Int12
     , lb           :: Int -- 1 bit
     , cpma, mb     :: Int12
     , mq           :: Int12
     , mem          :: Memory
     , halted       :: Bool
     } deriving (Eq, Ord, Show)

-- The machine memory associates addresses to integers
type Memory = M.Map Addr Int12

data Purpose = DataRead | DataWrite | InstrFetch
  deriving (Eq, Ord, Show, Enum)

initialState  = MS 0 0 0 0 0 0 0 0 initialMemory False
initialMemory = M.empty

data Value = VInstr { vInstr :: (Instr, Int12) } | VAddr { vAddr :: Addr }
  deriving (Eq, Ord, Show)

type Offset = Int12

-- |The 'Instr'uncation data type carries the decoded op, indirection,
-- memory page flags, and offset if applicable.
data Instr = AND { indirection :: Indirection
                 , page :: MemPage
                 , offset :: Offset }
           | TAD { indirection :: Indirection
                 , page :: MemPage
                 , offset :: Offset }
           | ISZ { indirection :: Indirection
                 , page :: MemPage
                 , offset :: Offset }
           | DCA { indirection :: Indirection
                 , page :: MemPage
                 , offset :: Offset }
           | JMS { indirection :: Indirection
                 , page :: MemPage
                 , offset :: Offset }
           | JMP { indirection :: Indirection
                 , page :: MemPage
                 , offset :: Offset }
           -- IO Ops
           | IOT IOOp
           -- End IO Ops
           | OP1 { cla :: Bool
                 , micros1 :: [MicroOp1]
                 , logical :: Maybe LogicalOp }
           | OP2 { cla :: Bool
                 , invertAndUnion :: Bool
                 , skips :: [SkipOp]
                 , micros2 :: [MicroOp2] }
           | OP3 { cla :: Bool
                 , micros3 :: [MicroOp3] }
           | UNK
           deriving (Eq, Ord, Show)

data IOOp  = KCF
           | KSF
           | KCC
           | KRS
           | KRB
           | TFL
           | TSF
           | TCF
           | TPC
           | TLS
  deriving (Eq, Ord, Show)

data InstructionType = MemOp | IOOp | MicroOp1 | MicroOp2 | MicroOp3 | UnknownOp
                     deriving (Eq, Ord, Show)

typeOf (IOT {}) = IOOp
typeOf (OP1 {}) = MicroOp1
typeOf (OP2 {}) = MicroOp2
typeOf (OP3 {}) = MicroOp3
typeOf _        = MemOp

data MicroOp1 =
  -- Microcodes
    CLL | CMA | CML | IAC
    deriving (Eq, Ord, Show)

data LogicalOp =
    RAR | RTR | RAL | RTL | BSW
    deriving (Eq, Ord, Show)

data SkipOp =
    SMA | SZA | SNL
    deriving (Eq, Ord, Show)

data MicroOp2 =
  -- Group 2
    OSR | HLT
    deriving (Eq, Ord, Show)

data MicroOp3 =
  -- Group 3
    MQL | MQA
    deriving (Eq, Ord, Show)

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
  let res = (a `op` b) `mod` 2^12
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
