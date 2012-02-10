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

type MonadCLI a = StateT DebugOp PDP8 a

-- However many liftings are needed for the PDP8 monad
-- embedded in MonadCLI
lift2 = lift

prompt = "# "

main 
  = runPDP8
  . flip runStateT noDebug
  $ loop

loop :: MonadCLI ()
loop = do
  i <- getInputLine prompt
  case i of
    Just ln -> do
      b <- process ln
      when b loop
    Nothing -> loop
  where 
    process :: String -> MonadCLI Bool
    process "quit" = return False
    process s = do
      processCmd s
      b <- lift2 isHalted
      return True
      
processCmd :: String -> MonadCLI ()
processCmd "reset" = lift2 reset
processCmd i | "step" `isPrefixOf` i = do
      let nr = parseNum (drop 4 i)
      replicateM_ nr doStep
processCmd "run" = run
processCmd i | "show " `isPrefixOf` i = do
      parseGetter (drop 5 i)
processCmd "cleardebug" = modify (const noDebug)
processCmd i | "debug " `isPrefixOf` i = do
      let op = parseGetter (filter isAlpha $ drop 6 i)
      modify (\s -> DS $ unDS s >> outputStr (i ++ ": ") >> op)
processCmd i | "load " `isPrefixOf` i = do
      let f = drop 5 i
      lift2 reset
      cont <- liftIO (readFile $ filter (/= '"') f)
      lift2 $ loadProgram (parseObj cont)
processCmd i | "set " `isPrefixOf` i = do
      setVal (drop 4 i)
processCmd _ = do
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


whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= \x -> if x then f else return ()

run :: MonadCLI ()
run = whenM (liftM not doStep) run

parseNum :: String -> Int
parseNum "" = 1
parseNum s = read . ('0':) . filter isDigit $ s

parseGetter :: String -> MonadCLI ()
parseGetter "stats" = lift2 getStats >>= outputStrLn . renderStats
parseGetter "logs"  = lift2 getStats >>= outputStrLn . renderLogs
parseGetter "mem"   = do
  m <- lift2 getMem
  outputStr . unlines
       . map (\(a,v) -> show4 (unAddr a) ++ " -> " ++ show4 v) 
       . M.toList $ m
parseGetter "pc"    = lift2 getPC    >>= prnt
parseGetter "ac"    = lift2 getAC    >>= prnt
parseGetter "l"     = lift2 getL     >>= prnt
parseGetter "sr"    = lift2 getSR    >>= prnt
parseGetter "ir"    = lift2 getIR    >>= prnt . decodeInstr
parseGetter "cpma"  = lift2 getCPMA  >>= prnt
parseGetter "mb"    = lift2 getMB    >>= prnt
parseGetter _       = outputStrLn "Unknown location for read operation!"

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
    "cpma" -> lift2 . setCPMA $ valO
    "mb"  -> lift2 . setMB $ valO
    _     -> outputStrLn "Unknown location for write operation!"

-- Returns true if halted
doStep :: MonadCLI Bool
doStep = do
  h <- lift2 isHalted
  if h then return True
    else do b <- lift2 step
            s <- get
            unDS s
            res <- lift2 isHalted
            when res (outputStrLn "Program HALTed")
            return res

prnt :: (Show a) => a -> MonadCLI ()
prnt = outputStrLn . show

readO :: (Read a, Integral a) => String -> a
readO = fst . head . readOct