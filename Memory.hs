module Memory 
  ( renderMemLog
  , read, store, fetch
  )where

import Parse (decodeInstr, Addr(..))
import qualified Data.Map as M
import Numeric (showOct)

renderMemLog :: MemoryLog -> String
renderMemLog = unlines . map (\(p,a) -> show (fromEnum p) ++ " " ++ showOct (unAddr a) ++ "\n")

tel :: Purpose -> Addr -> PDP8 ()
tel p a = lift (tell (p,a))

read :: (MonadState Memory m, MonadWriter String m) => Addr -> m Int
read a = tel DataRead a >> gets (M.findWithDefault 0 a)

store :: (MonadState Memory m, MonadWriter String m) => Addr -> Int -> m ()
store a i = tel DataWrite a >> modify (M.insert a i)

fetch :: (MonadState Memory m, MonadWriter String m) => Addr -> m Instr
fetch a = tel InstrFetch >> liftM decodeInstr (gets a)

-- Only needed if we don't force this property
-- by some sort of smart constructor.  Good idea?
sanityCheck :: MonadState Memory m => m ()
sanityCheck = do
  m <- get
  let as = M.keys m
      es = M.elems m
  if not (all addrValid as)
     then
     else if not (all memValid es)
           then 
           else return ()
 where
 addrValid a = a < Addr (2^12)
 memValid  e = e < 2^12 && e > 0
