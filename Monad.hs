module Monad 
  ( evalPDP8
  , logPDP8
  , execPDP8
  , runPDP8
  , PDP8
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Types

newtype PDP8 a = PDP8 { unPDP8 :: StateT MachineState (WriterT Branches (WriterT MemoryLog IO)) a }

evalPDP8 :: PDP8 a -> IO a
evalPDP8 = liftM (\(m,b,s,a) -> a) . runPDP8

logPDP8 :: PDP8 a -> IO (MemoryLog,Branches)
logPDP8 = liftM (\(m,b,s,a) -> (m,b)) . runPDP8

execPDP8 :: PDP8 a -> IO (MemoryLog, Branches, MachineState)
execPDP8 = liftM (\(m,b,s,a) -> (m,b,s)) . runPDP8

runPDP8 :: PDP8 a -> IO (MemoryLog, Branches, MachineState, a)
runPDP8 = liftM (\(((a,st),br), ml) -> (ml, br, st,a))
        . runWriterT
        . runWriterT
        . flip runStateT initialState
        . unPDP8
