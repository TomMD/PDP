module Execute
       ( step
       ) where

import Monad
import Stats
import Types
import Memory

step :: PDP8 ()
step = do
  p <- getPC
  i <- fetch (Addr p)
  incStats i
  stepInstr i
  modPC (+1) -- FIXME adding just 1, PC is in words I hope?

stepInstr :: Instr -> PDP8 ()
stepInstr i@(Instr (PDPOpMem _)    _ _ _ _) = stepInstrMemOp i
stepInstr i@(Instr (PDPOpIO _)     _ _ _ _) = stepInstrIOOp i
stepInstr i@(Instr (PDPOpMicro1 _) _ _ _ _) = stepInstrMicro1 i
stepInstr i@(Instr (PDPOpMicro2 _) _ _ _ _) = stepInstrMicro2 i
stepInstr i@(Instr (PDPOpMicro3 _) _ _ _ _) = stepInstrMicro3 i
stepInstr i@(Instr UnknownOp       _ _ _ _) = stepInstrUnknown i


stepInstrMemOp :: Instr -> PDP8 ()
stepInstrMemOp = undefined

stepInstrIOOp :: Instr -> PDP8 ()
stepInstrIOOp = undefined

stepInstrMicro1 :: Instr -> PDP8 ()
stepInstrMicro1 = undefined

stepInstrMicro2 :: Instr -> PDP8 ()
stepInstrMicro2 = undefined

stepInstrMicro3 :: Instr -> PDP8 ()
stepInstrMicro3 = undefined

stepInstrUnknown   :: Instr -> PDP8 ()
stepInstrUnknown _ = return () -- FIXME make a new log in the transformer stack and log this event?