{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Execute -- step :: PDP8 Bool (True == terminate?)
import Memory (loadProgram)
import Monad
import Parse
import Prelude hiding (catch)
import Stats
import Types
import Util

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Data.Char (toLower, isDigit, isSpace, isAlpha)
import Data.List
import Numeric
import System.Console.Haskeline.Class
import qualified Data.Map as M

newtype DebugOp = DS { unDS :: MonadCLI () }

noDebug :: DebugOp
noDebug = DS (return ())

type MonadCLI a = StateT DebugOp (HaskelineT PDP8) a

-- However many liftings are needed for the PDP8 monad
-- embedded in MonadCLI
lift2 = lift . lift

prompt = "\n> "

main 
  = runPDP8
  . runHaskelineT defaultSettings
  . flip runStateT noDebug
  $ loop

loop :: MonadCLI ()
loop = do
  i <- getInputLine prompt
  case i of
    Just ln -> do
      b <- processCmd ln
      when b loop
    Nothing -> loop
  where 
    rt = return True :: MonadCLI Bool
    processCmd :: String -> MonadCLI Bool
    processCmd i | "step" `isPrefixOf` i = do
      let nr = parseNum (drop 4 i)
      xs <- replicateM nr doStep
      return (and xs)
    processCmd "run" = run >> return False
    processCmd i | "show " `isPrefixOf` i = do
      parseGetter (drop 5 i) >> rt
    processCmd "cleardebug" = modify (const noDebug) >> rt
    processCmd i | "debug " `isPrefixOf` i = do
      let op = parseGetter (drop 6 i)
      modify (\s -> DS $ unDS s >> outputStr (i ++ ": ") >> op)
      rt
    processCmd i | "load " `isPrefixOf` i = do
      let f = drop 5 i
      lift2 reset
      cont <- liftIO (readFile f)
      lift2 $ loadProgram (parseObj cont)
      rt
    processCmd i | "set " `isPrefixOf` i = do
      setVal (drop 4 i)
      rt
    processCmd "help" = do
      outputStrLn $ unlines
        [ "PDP8 Interpreter (by Thomas DuBuisson & Garrett Morris)"
        , "Commands:"
        , "\tstep [n]          Step 1 (or a given number) of instructions."
        , "\trun               Run until termination"
        , "\tshow <loc>        Show a particular location's value"
        , "\tset <loc>         Set the value of a particular location"
        , "\tload <file>       Load an octal .obj file"
        , "\tdebug <loc>       Each step, print the value at <location>"
        , "\tcleardebug        Don't print any value on each step"
        , "-----------"
        , "Locations:"
        , "\tstats             Statistics (not settable!)"
        , "\tlogs              Log files  (not settable!)"
        , "\tmem               Memory (set with 'set mem <octAddr> <oct>')"
        , "\t<reg>             Register (pc,ac,l,sr,ir,cpma,mb)"
        ]
      rt


whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= \x -> if x then f else return ()

run :: MonadCLI ()
run = whenM doStep run

parseNum :: String -> Int
parseNum = read . ('0':) . filter isDigit

parseGetter :: String -> MonadCLI ()
parseGetter "stats" = lift2 getStats >>= outputStrLn . renderStats
parseGetter "logs"  = lift2 getStats >>= outputStrLn . renderLogs
parseGetter "mem"   = lift2 getMem   >>= prnt
parseGetter "pc"    = lift2 getPC    >>= prnt
parseGetter "ac"    = lift2 getAC    >>= prnt
parseGetter "l"     = lift2 getL     >>= prnt
parseGetter "sr"    = lift2 getSR    >>= prnt
parseGetter "ir"    = lift2 getIR    >>= prnt . decodeInstr
parseGetter "cpma"  = lift2 getCPMA  >>= prnt
parseGetter "mb"    = lift2 getMB    >>= prnt

setVal :: String -> MonadCLI ()
setVal str =
  let (locS,valS) = break isDigit str
      valO = readO valS
      (locOS,valO2S) = break isSpace valS
      locO  = Addr . readO $ locOS
      valO2 = readO valO2S
  in case filter isAlpha locS of
    "mem" -> lift2 (modMem (M.insert locO valO2))
    "pc"  -> lift2 . setPC $ valO
    "ac"  -> lift2 . setAC $ valO
    "l"   -> lift2 . setL . fromIntegral $valO
    "sr"  -> lift2 . setSR $ valO
    "ir"  -> lift2 . setIR $ valO

doStep :: MonadCLI Bool
doStep = do
  h <- lift2 isHalted
  if h then return False
    else do b <- lift2 step
            s <- get
            unDS s
            lift2 isHalted

prnt :: (Show a) => a -> MonadCLI ()
prnt = outputStrLn . show

instance MonadException PDP8 where
  catch m h = PDP8 $ catch (unPDP8 m) 
                           (\e -> unPDP8 (h e))
  block = PDP8 . block . unPDP8
  unblock = PDP8 . unblock . unPDP8
  
instance MonadIO PDP8 where
  liftIO = PDP8 . liftIO
  
readO :: (Read a, Integral a) => String -> a
readO = fst . head . readOct