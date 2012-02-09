{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Monad
  ( -- * Monad Execution
    evalPDP8
  , logPDP8
  , execPDP8
  , runPDP8
  , PDP8(..)
    -- * Utilities
  , reset
  , logMem
  , logBranch
  , getBranchLog, setBranchLog, modBranchLog
  , getMemoryLog, setMemoryLog, modMemoryLog
  , modMem, getMem, setMem
  , modStats, getStats, setStats
  , getAC, setAC, modAC
  , getPC, setPC, modPC
  , getL, setL, modL
  , getSR, setSR, modSR
  , getIR, setIR, modIR
  ) where

import Control.Monad.State
import Control.Monad.Writer

import Types

newtype PDP8 a = PDP8 { unPDP8 :: StateT MachineState (StateT Stats IO) a }
  deriving (MonadState MachineState, Monad)

reset :: PDP8 ()
reset = setStats initialStats >> put initialState

logBranch :: Addr -> Addr -> PDP8 ()
logBranch src target =
  modBranchLog (++ [(src,target)])

logMem :: Purpose -> Addr -> PDP8 ()
logMem p a = modMemoryLog (++ [(p,a)])

getMemoryLog :: PDP8 MemoryLog
getMemoryLog = PDP8 $ liftM memoryLog (lift get)

setMemoryLog :: MemoryLog -> PDP8 ()
setMemoryLog = modMemoryLog . const

modMemoryLog :: (MemoryLog -> MemoryLog) -> PDP8()
modMemoryLog f = PDP8 $ lift $ modify (\st -> st { memoryLog = f (memoryLog st) } )

getBranchLog :: PDP8 BranchLog
getBranchLog = PDP8 $ liftM branchLog (lift get)

setBranchLog :: BranchLog -> PDP8 ()
setBranchLog = modBranchLog . const

modBranchLog :: (BranchLog -> BranchLog) -> PDP8 ()
modBranchLog f = PDP8 $ lift $ modify (\st -> st { branchLog = f (branchLog st) } )

modMem :: (Memory -> Memory) -> PDP8 ()
modMem f = modify (\s -> s { mem = f (mem s)})

getMem :: PDP8 Memory
getMem = gets mem

setMem :: Memory -> PDP8 ()
setMem = modMem . const

modStats :: (Stats -> Stats) -> PDP8 ()
modStats = PDP8 . lift . modify

getStats :: PDP8 Stats
getStats = PDP8 (lift get)

setStats :: Stats -> PDP8 ()
setStats = PDP8 . lift . put

getPC, getSR, getAC, getIR, getCPMA, getMB :: PDP8 Int12
getPC   = gets pc
getSR   = gets sr
getAC   = gets ac
getIR   = gets ir
getCPMA = gets cpma
getMB   = gets mb

modPC, modSR, modAC, modCPMA, modMB :: (Int12 -> Int12) -> PDP8 ()
modPC f   = modify (\s -> s { pc = f (pc s) })
modSR f   = modify (\s -> s { sr = f (sr s) })
modAC f   = modify (\s -> s { ac = f (ac s) })
modIR f   = modify (\s -> s { ir = f (ir s) })
modCPMA f = modify (\s -> s { cpma = f (cpma s) })
modMB f   = modify (\s -> s { mb = f (mb s) })

setPC, setSR, setAC, setCPMA, setMB :: Int12 -> PDP8 ()
setPC x   = modPC (const x)
setSR x   = modSR (const x)
setAC x   = modAC (const x)
setIR x   = modIR (const x)
setCPMA x = modCPMA (const x)
setMB x   = modMB (const x)

getL :: PDP8 Int
getL   = gets lb

setL :: Int -> PDP8 ()
setL x = get >>= \s -> put s { lb = x }

modL :: (Int -> Int) -> PDP8 ()
modL f = getL >>= setL . f

evalPDP8 :: PDP8 a -> IO a
evalPDP8 = liftM (\(_,_,a) -> a) . runPDP8

logPDP8 :: PDP8 a -> IO Stats
logPDP8 = liftM (\(_,s,_) -> s) . runPDP8

execPDP8 :: PDP8 a -> IO (MachineState,Stats)
execPDP8 = liftM (\(s,t,a) -> (s,t)) . runPDP8

runPDP8 :: PDP8 a -> IO (MachineState, Stats, a)
runPDP8 = liftM (\((a,st),stats) -> (st,stats,a))
        . flip runStateT initialStats
        . flip runStateT initialState
        . unPDP8