{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monad 
  ( -- * Monad Execution
    evalPDP8
  , logPDP8
  , execPDP8
  , runPDP8
  , PDP8
    -- * Utilities
  , logMem
  , logBranch
  , modMem
  , modStats
  , getPC, setPC, modPC
  , getL, setL, modL
  , getSR, setSR, modSR
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Types

newtype PDP8 a = PDP8 { unPDP8 :: StateT MachineState (StateT Stats (WriterT Branches (WriterT MemoryLog IO))) a }
  deriving (MonadState MachineState, Monad, MonadWriter Branches)

logBranch :: Addr -> Addr -> PDP8 ()
logBranch src target = tell [(src,target)]

logMem :: Purpose -> Addr -> PDP8 ()
logMem p a = PDP8 $ lift $ lift $ lift (tell [(p,a)])

modMem :: (Memory -> Memory) -> PDP8 ()
modMem f = modify (\s -> s { mem = f (mem s)})

modStats :: (Stats -> Stats) -> PDP8 ()
modStats = PDP8 . lift . modify

getPC, getSR, getAC :: PDP8 Int12
getPC   = gets pc
getSR   = gets sr
getAC   = gets ac

setPC, setSR, setAC :: Int12 -> PDP8 ()
setPC x = get >>= \s -> put s { pc = x }
setSR x = get >>= \s -> put s { sr = x }
setAC x = get >>= \s -> put s { ac = x }

modPC, modSR, modAC :: (Int12 -> Int12) -> PDP8 ()
modPC f = getPC >>= setPC . f
modSR f = getSR >>= setSR . f
modAC f = getAC >>= setAC . f

getL   = gets lb
setL x = get >>= \s -> put s { lb = x }

modL :: (Int -> Int) -> PDP8 ()
modL f = getL >>= setL . f

evalPDP8 :: PDP8 a -> IO a
evalPDP8 = liftM (\(m,b,s,_,a) -> a) . runPDP8

logPDP8 :: PDP8 a -> IO (MemoryLog,Branches)
logPDP8 = liftM (\(m,b,s,_,a) -> (m,b)) . runPDP8

execPDP8 :: PDP8 a -> IO (MemoryLog, Branches, MachineState,Stats)
execPDP8 = liftM (\(m,b,s,t,a) -> (m,b,s,t)) . runPDP8

runPDP8 :: PDP8 a -> IO (MemoryLog, Branches, MachineState, Stats, a)
runPDP8 = liftM (\((((a,st),stats),br), ml) -> (ml, br, st, stats,a))
        . runWriterT
        . runWriterT
        . flip runStateT initialStats
        . flip runStateT initialState
        . unPDP8
