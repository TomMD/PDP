module Memory
  ( load, store, fetchInstruction
  , effectiveAddr
  , loadProgram
  )where

import Data.Bits
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Numeric (showOct)
import Control.Monad.State (gets)
import Control.Monad (liftM)

import Arch (addrMode, AddrMode(..))
import Monad
import Parse (decodeInstr)
import Stats
import Types
import Util

findWithDefault d k m = fromMaybe d (M.lookup k m)

-- Memory Operations --

-- Load a value from a given address.  If this is for instructions, use 'fetch'
-- instead.
load :: Addr -> PDP8 Int12
load a =
  logMem DataRead a >>
  gets (findWithDefault 0 a . mem)

-- Store a 'Int12' at a given memory address
store :: Addr -> Int12 -> PDP8 ()
store a i =
  logMem DataWrite a >>
  modMem (M.insert a i)

-- Fetch and decode an instruction from a memory address; returns the decoded
-- instruction and stores it in IR.
fetchInstruction :: Addr -> PDP8 Instr
fetchInstruction a = do
  logMem InstrFetch a
  rawInstr <- gets (findWithDefault 0 a . mem)
  setIR rawInstr
  let i = decodeInstr rawInstr
  incStats i
  return i

-- Given an instruction aid its address, returns the effective address while
-- performing any needed auto-incrementing.
effectiveAddr :: Instr -> Int12 -> PDP8 Addr
effectiveAddr i iaddr =
    case typeOf i of
      MemOp -> effectiveAddr' (page i) (offset i) (addrMode i)
      _ -> error "Can not compute the effective address of a non-memory operation.\
                 \ The parser must have failed and resulted in an invalid AST for a\
                 \ given instruction."
    where effectiveAddr' ZeroPage off ModeIndirect =
              liftM Addr (load (Addr off))
          effectiveAddr' ZeroPage off ModeDirect =
              return (Addr off)
          effectiveAddr' ZeroPage off ModeAutoIndexing =
              do let ptr = Addr off
                 addr <- load ptr
                 store (Addr off) (addr + 1)
                 return (Addr (addr+1))
          effectiveAddr' CurrentPage off mode =
              let eAddr = Addr (off .|. iaddr .&. ob 111110000000)
              in case mode of
                   ModeIndirect -> liftM Addr (load eAddr)
                   ModeDirect   -> return eAddr
                   ModeAutoIndexing -> error "addrMode: autoindexing with CurrentPage"

-- Given a list of values produced by parseObj, load the data into
-- memory; doesn't affect memory loads or access counters.
loadProgram :: [Value] -> PDP8 ()
loadProgram values =
 do go (Addr 0) values
    setPC (startAddr values)
 where
   go a []     = return ()
   go _ (VAddr a  : vs) = go a vs
   go a (VInstr (_,i) : vs) = do
     modMem (M.insert a i)
     go (incrAddr a) vs
   incrAddr = Addr . (+1) . unAddr

   startAddr (VAddr (Addr a) : _)   = a
   startAddr (VInstr _ : vs) = startAddr vs
   startAddr []              = 0
