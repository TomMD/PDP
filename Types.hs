module Types 
  ( -- * Aliases
    Branches, MemoryLog, Register, Memory
    -- * Types
  , MachineState(..)
  , Purpose
  , Stats(..)
    -- * Helpers
  , initialState, initialMemory) where

import Parse
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
  MS { pc                 :: Register
     , mb,ac,cpma,mq,csr  :: Register
     , ir   :: Int -- 3 bit instruction register
     , lb   :: Int -- 1 bit
     , mem  :: Memory
     } deriving (Eq, Ord, Show)

-- The machine memory associates addresses to integers
type Memory = M.Map Addr Int

data Purpose = DataRead | DataWrite | InstrFetch
  deriving (Eq, Ord, Show, Enum)

initialState  = MS 0 0 0 0 0 0 0 0 initialMemory
initialMemory = M.empty

