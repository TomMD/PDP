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
  , modLAC
  , getSR, setSR, modSR
  , getIR, setIR, modIR
  , getMQ, setMQ, modMQ
  , clearHalt, halt, isHalted
   -- * PDP-8 operations supporting IOT
  , setTeleprinterFlag, getTeleprinterFlag
  , setKeyboardFlag, getKeyboardFlag, getKB
   -- * Re-exported IO Operations
  , getInputChar, getInputLine, outputStr, outputStrLn
  ) where

import Prelude hiding (catch)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.DList as D
import Data.Maybe
import System.Console.Haskeline.Class
import Control.Monad.IO.Class

import qualified Data.DList as D

import Types

newtype PDP8 a = PDP8 { unPDP8 :: StateT MachineState (StateT Stats (HaskelineT IO)) a }
  deriving (MonadState MachineState, Monad, Functor)

instance MonadHaskeline PDP8 where
  getInputLine = PDP8 . getInputLine
  getInputChar = PDP8 . getInputChar
  outputStr    = PDP8 . outputStr
  outputStrLn  = PDP8 . outputStrLn

instance MonadException PDP8 where
  catch m h = PDP8 $ catch (unPDP8 m)
                           (\e -> unPDP8 (h e))
  block = PDP8 . block . unPDP8
  unblock = PDP8 . unblock . unPDP8

instance MonadIO PDP8 where
  liftIO = PDP8 . liftIO

clearHalt, halt :: PDP8 ()
halt      = modify (\s -> s { halted = True  })
clearHalt = modify (\s -> s { halted = False })

isHalted :: PDP8 Bool
isHalted  = gets halted

reset :: PDP8 ()
reset = setStats initialStats >> put initialState

logBranch :: Addr -> Addr -> PDP8 ()
logBranch src target =
  modBranchLog (`D.snoc` (src,target))

logMem :: Purpose -> Addr -> PDP8 ()
logMem p a = modMemoryLog (flip D.snoc (p,a))

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

getPC, getSR, getAC, getIR, getMQ :: PDP8 Int12
getPC   = gets pc
getSR   = gets sr
getAC   = gets ac
getIR   = gets ir
getMQ   = gets mq

modPC, modSR, modAC, modMQ :: (Int12 -> Int12) -> PDP8 ()
modPC f   = modify (\s -> s { pc = f (pc s) })
modSR f   = modify (\s -> s { sr = f (sr s) })
modAC f   = modify (\s -> s { ac = f (ac s) })
modIR f   = modify (\s -> s { ir = f (ir s) })
modMQ f   = modify (\s -> s { mq = f (mq s) })

setPC, setSR, setAC, setMQ :: Int12 -> PDP8 ()
setPC x   = modPC (const x)
setSR x   = modSR (const x)
setAC x   = modAC (const x)
setIR x   = modIR (const x)
setMQ x   = modMQ (const x)

getL :: PDP8 Int
getL   = gets lb

setL :: Int -> PDP8 ()
setL x = get >>= \s -> put s { lb = x }

modL :: (Int -> Int) -> PDP8 ()
modL f = getL >>= setL . f

modLAC :: ((Int,Int12) -> (Int,Int12)) -> PDP8 ()
modLAC f = do l <- getL
              ac <- getAC
              let (l', ac') = f (l, ac)
              setL l'
              setAC ac'

-- The teleprinter flag is a dummy.  Our printer runs infinately fast,
-- so thinks the program being interpreted.
setTeleprinterFlag :: Bool -> PDP8 ()
setTeleprinterFlag _ = return ()

getTeleprinterFlag :: PDP8 Bool
getTeleprinterFlag = return True

-- The keyboard flag is also a dummy - there is always keyboard IO to be read!
setKeyboardFlag :: Bool -> PDP8 ()
setKeyboardFlag _ = return ()

getKeyboardFlag :: PDP8 Bool
getKeyboardFlag = return True

-- We currently print a prompt asking the user for a character, but that can change.
getKB :: PDP8 Int12
getKB = liftM (fromIntegral . fromEnum . fromMaybe 'X') (getInputChar "->")

evalPDP8 :: PDP8 a -> IO a
evalPDP8 = liftM (\(_,_,a) -> a) . runPDP8

logPDP8 :: PDP8 a -> IO Stats
logPDP8 = liftM (\(_,s,_) -> s) . runPDP8

execPDP8 :: PDP8 a -> IO (MachineState,Stats)
execPDP8 = liftM (\(s,t,a) -> (s,t)) . runPDP8

runPDP8 :: PDP8 a -> IO (MachineState, Stats, a)
runPDP8 = liftM (\((a,st),stats) -> (st,stats,a))
        . runHaskelineT defaultSettings
        . flip runStateT initialStats
        . flip runStateT initialState
        . unPDP8
