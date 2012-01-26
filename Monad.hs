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
  , getPC, setPC, modPC
  , getL, setL, modL
  , getSR, setSR, modSR
  ) where

import Control.Monad.State
import Control.Monad.Writer
import Types

newtype PDP8 a = PDP8 { unPDP8 :: StateT MachineState (WriterT Branches (WriterT MemoryLog IO)) a }
  deriving (MonadState MachineState, Monad, MonadWriter Branches)

logBranch :: Addr -> Addr -> PDP8 ()
logBranch src target = tell [(src,target)]

logMem :: Purpose -> Addr -> PDP8 ()
logMem p a = PDP8 $ lift $ lift (tell [(p,a)])

modMem :: (Memory -> Memory) -> PDP8 ()
modMem f = modify (\s -> s { mem = f (mem s)})

getPC, getSR :: PDP8 Int12
getPC   = gets pc
getSR   = gets sr

setPC, setSR :: Int12 -> PDP8 ()
setPC x = get >>= \s -> put s { pc = x }
setSR x = get >>= \s -> put s { sr = x }

modPC, modSR :: (Int12 -> Int12) -> PDP8 ()
modPC f = getPC >>= setPC . f
modSR f = getSR >>= setSR . f

getL   = gets lb
setL x = get >>= \s -> put s { lb = x }

modL :: (Int -> Int) -> PDP8 ()
modL f = getL >>= setL . f

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
