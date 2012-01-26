module Memory 
  ( renderMemLog
  , load, store, fetch
  , loadProgram
  )where

import qualified Data.Map as M
import Numeric (showOct)
import Control.Monad.State (gets)

import Monad
import Parse (decodeInstr, encodeInstr)
import Types

renderMemLog :: MemoryLog -> String
renderMemLog = unlines . map (\(p,a) -> show (fromEnum p) ++ " " ++ showOct (unAddr a) "" ++ "\n")

-- Utilities --
incPC :: PDP8 ()
incPC = modPC (+1)

-- End Utilities --

-- Memory Operations --
load :: Addr -> PDP8 Int12
load a =
  logMem DataRead a >>
  gets (M.findWithDefault 0 a . mem)

store :: Addr -> Int12 -> PDP8 ()
store a i =
  logMem DataWrite a >> 
  modMem (M.insert a i)

fetch :: Addr -> PDP8 Instr
fetch a = do
  logMem InstrFetch a
  i <- gets (decodeInstr. M.findWithDefault 0 a . mem)
  incPC
  return i

-- |Given a list of values produced by parseObj, load the data into
-- memory. N.B. 'loadProgram' won't adjust the memory access counters!
loadProgram :: [Value] -> PDP8 ()
loadProgram values = go (Addr 0) values
 where
   go a []     = return ()
   go _ (VAddr a  : vs) = go a vs
   go a (VInstr i : vs) = do
     modMem (M.insert a (encodeInstr i))
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
