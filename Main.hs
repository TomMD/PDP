module Main where

import qualified CLI as CLI
import Execute -- step :: PDP8 Bool (True == terminate?)
import Memory (loadProgram, store)
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
import Data.Maybe (fromJust)
import Numeric
import System.Console.Haskeline.Class
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as M

newtype DebugOp = DS { unDS :: MonadCLI () }

noDebug :: DebugOp
noDebug = DS (return ())

type MonadCLI a = StateT DebugOp PDP8 a

prompt = "# "

main
  = do mo <- CLI.getOptions
       case mo of
         Nothing -> runPDP8
                    . flip runStateT noDebug
                    $ loop
         Just o  ->
             runPDP8 $ flip runStateT noDebug $
             do obj <- liftIO (readFile (fromJust (CLI.input o)))
                lift (loadProgram (parseObj obj))
                mapM_ addDebug (CLI.debug o)
                case CLI.startingPC o of
                  Nothing -> return ()
                  Just pc -> lift (setPC pc)
                run
                mapM_ parseGetter (CLI.showAtEnd o)
                case CLI.memoryLog o of
                  Nothing -> return ()
                  Just fn -> liftIO . writeFile fn .
                             renderMemoryLog (CLI.descriptive o) =<<
                             lift getMemoryLog
                case CLI.branchLog o of
                  Nothing -> return ()
                  Just fn -> liftIO . writeFile fn .
                             renderBranchLog =<<
                             lift getBranchLog
                case CLI.statistics o of
                  Nothing -> return ()
                  Just fn -> liftIO . writeFile fn .
                             renderStats =<<
                             lift getStats

loop :: MonadCLI ()
loop = do
  i <- fmap (fmap $ reverse . dropWhile isSpace . reverse) (getInputLine prompt)
  case i of
    Just ln -> do
      b <- process ln
      when b loop
    Nothing -> loop
  where
    process :: String -> MonadCLI Bool
    process "" = return True
    process "quit" = return False
    process s = do
      processCmd s
      b <- lift isHalted
      return True

addDebug :: String -> MonadCLI ()
addDebug str = modify (\s -> DS $ unDS s >> outputStr (str ++ ": ") >> op)
    where op = parseGetter (filter isAlpha str)

processCmd :: String -> MonadCLI ()
processCmd "reset" = lift reset
processCmd "addrload" = lift (setPC =<< getSR)
processCmd "deposit" =
      lift $ do a <- getPC
                store (Addr a) =<< getSR
                modPC (1+)
processCmd i | "step" `isPrefixOf` i = do
      let nr = parseNum (drop 4 i)
      replicateM_ nr doStep
processCmd "run" = run
processCmd i | "show " `isPrefixOf` i = do
      parseGetter (drop 5 i)
processCmd "cleardebug" = modify (const noDebug)
processCmd i | "debug " `isPrefixOf` i =
      addDebug (drop 6 i)
processCmd i | "load " `isPrefixOf` i = do
      let f = drop 5 i
      lift reset
      let hdl :: SomeException -> MonadCLI ()
          hdl _ = outputStrLn "Error loading object file!"
      catch (do
        cont <- liftIO (readFile $ filter ('"' /=) f)
        lift $ loadProgram (parseObj cont)) hdl
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
        , "\treset             Reset all memory, registers, and stats (to zero)"
        , "\taddrload          Loads the contents of SR into PC (substituting for the unimplemented CPMA)"
        , "\tdeposit           Loads the contents of SR into the memory address in PC (substituting for"
        , "\t                  CPMA) and increments PC"
        , "-----------"
        , "Locations:"
        , "\tstats             Statistics (not settable!)"
        , "\tlogs              Log files  (not settable!)"
        , "\tmem               Memory (set with 'set mem <octAddr> <oct>')"
        , "\t<reg>             Register (pc,ac,l,sr,ir)"
        ]


whenM :: Monad m => m Bool -> m () -> m ()
whenM b f = b >>= \x -> if x then f else return ()

run :: MonadCLI ()
run = whenM (fmap not doStep) run

parseNum :: String -> Int
parseNum "" = 1
parseNum s = read . ('0':) . filter isDigit $ s

parseGetter :: String -> MonadCLI ()
parseGetter "stats" = lift getStats >>= outputStrLn . renderStats
parseGetter "logs"  = lift getStats >>= outputStrLn . renderLogs
parseGetter "mem"   = do
  m <- lift getMem
  outputStr . unlines
       . map (\(a,v) -> show4 (unAddr a) ++ " -> " ++ show4 v)
       . M.toList $ m
parseGetter "pc"    = lift getPC    >>= prnt
parseGetter "ac"    = lift getAC    >>= prnt
parseGetter "l"     = lift getL     >>= prnt
parseGetter "sr"    = lift getSR    >>= prnt
parseGetter "ir"    = lift getIR    >>= prnt . decodeInstr
parseGetter _       = outputStrLn "Unknown location for read operation!"

setVal :: String -> MonadCLI ()
setVal str =
  let (locS,valS) = break isDigit str
      valO = octS valS
      (locOS,valO2S) = break isSpace valS
      locO  = Addr . octS $ locOS
      valO2 = octS valO2S
  in case filter isAlpha locS of
    "mem" -> lift (modMem (M.insert locO valO2))
    "pc"  -> lift . setPC $ valO
    "ac"  -> lift . setAC $ valO
    "l"   -> lift . setL . fromIntegral $valO
    "sr"  -> lift . setSR $ valO
    "ir"  -> lift . setIR $ valO
    _     -> outputStrLn "Unknown location for write operation!"

-- Returns true if halted
doStep :: MonadCLI Bool
doStep = do
  h <- lift isHalted
  if h then return True
    else do b <- lift step
            s <- get
            unDS s
            res <- lift isHalted
            when res (outputStrLn "Program HALTed")
            return res

prnt :: (Show a) => a -> MonadCLI ()
prnt = outputStrLn . show

octS :: Integral a => String -> a
octS = oct . (read :: String -> Int)
