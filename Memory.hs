module Memory
  ( load, store, fetch
  , effectiveAddr
  , loadProgram
  )where

import Data.Bits
import qualified Data.Map as M
import Numeric (showOct)
import Control.Monad.State (gets)
import Control.Monad (liftM)

import Arch (addrMode, AddrMode(..))
import Monad
import Parse (decodeInstr)
import Types
import Util

-- Utilities --
incPC :: PDP8 ()
incPC = modPC (+1)

-- End Utilities --

-- Memory Operations --

-- |load a value from a given address.  If this is for instructions, use 'fetch' instead.
load :: Addr -> PDP8 Int12
load a =
  logMem DataRead a >>
  gets (M.findWithDefault 0 a . mem)

-- |Store a 'Int12' at a given memory address
store :: Addr -> Int12 -> PDP8 ()
store a i =
  logMem DataWrite a >>
  modMem (M.insert a i)

-- |Fetch and decode an instruction from a memory address
fetch :: Addr -> PDP8 Instr
fetch a = do
  logMem InstrFetch a
  i <- gets (decodeInstr. M.findWithDefault 0 a . mem)
  incPC
  return i

-- |Given the newly fetched instruction, which must be a memory
-- operation, returns the effective address while performing any
-- needed auto-incrementing (costing an additional load and store - correct? TODO CHECKME).
--
-- FIXME This should be called before (I think) incrementing the PC??
-- Probably before!  check the behavior of a PDP8 in the
-- documentation!
effectiveAddr :: Instr -> PDP8 Addr
effectiveAddr i =
    case typeOf i of
      MemOp -> effectiveAddr' (page i) (offset i) (addrMode i)
      _ -> error "Can not compute the effective address of a non-memory operation.\
                 \ The parser must have failed and resulted in an invalid AST for a\
                 \ given instruction."
    where effectiveAddr' ZeroPage off ModeIndirect =
              liftM Addr (load (Addr off))
          effectiveAddr' ZeroPage off ModeDirect =
              do pcAddr <- getPC
                 let ea = (pcAddr .&. ob 111110000000) .|. off
                 return (Addr ea)
          effectiveAddr' ZeroPage off ModeAutoIndexing =
              do let ptr = Addr off
                 addr <- load ptr
                 store (Addr off) (addr + 1)
                 return (Addr (addr+1))
          effectiveAddr' CurrentPage off mode =
              do pcAddr <- liftM (.&. ob 111110000000) getPC
                 let eAddr = Addr (off .|. pcAddr)
                 case mode of
                   ModeIndirect -> liftM Addr (load eAddr)
                   ModeDirect   -> return eAddr
                   ModeAutoIndexing -> error "addrMode is broken - autoindexing is not valid with the CurrentPage bit set."

-- |Given a list of values produced by parseObj, load the data into
-- memory. N.B. 'loadProgram' won't adjust the memory access counters!
loadProgram :: [Value] -> PDP8 ()
loadProgram values = go (Addr 0) values
 where
   go a []     = return ()
   go _ (VAddr a  : vs) = go a vs
   go a (VInstr (_,i) : vs) = do
     modMem (M.insert a i)
     go (incrAddr a) vs
   incrAddr = Addr . (+1) . unAddr

-- Only needed if we don't force this property
-- by some sort of smart constructor.  Good idea?
sanityCheck :: PDP8 ()
sanityCheck = do
  m <- gets mem
  let as = M.keys m
      es = M.elems m
  if not (all addrValid as)
     then error $ "Invalid addresses: " ++ show m
     else if not (all memValid es)
           then error $ "Invalid memory: " ++ show m
           else return ()
 where
 addrValid a = a < Addr (2^12)
 memValid  e = (-1)*2^11 < e && e < 2^11 - 1
