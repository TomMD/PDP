module CLI where

import Data.List (intercalate)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

data Options = O { input      :: Maybe String
                 , debug      :: [String]
                 , showAtEnd  :: [String]
                 , memoryLog  :: Maybe String
                 , branchLog  :: Maybe String
                 , statistics :: Maybe String
                 , showHelp   :: Bool }

defaultOptions = O Nothing [] [] Nothing Nothing Nothing False

options :: [OptDescr (Options -> Options)]
options = [ Option ['d'] ["debug"]
                   (ReqArg (\x o -> o { debug = debug o ++ [x] }) "VALUE")
                   "Display VALUE after each step (where VALUE is stats, log, mem, pc, ac, l, sr, ir)"
          , Option ['e'] ["show-at-end"]
                   (ReqArg (\x o -> o { showAtEnd = showAtEnd o ++ [x] }) "VALUE")
                   "Display VALUE at end of run (where VALUE is stats, log, mem, pc, ac, l, sr, ir)"
          , Option ['m'] ["memory-log"]
                   (ReqArg (\x o -> o { memoryLog = Just x }) "FILE")
                   "Write memory log to FILE"
          , Option ['b'] ["branch-log"]
                   (ReqArg (\x o -> o { branchLog = Just x }) "FILE")
                   "Write branch log to FILE"
          , Option ['s'] ["statistics"]
                   (ReqArg (\x o -> o { statistics = Just x }) "FILE")
                   "Write statistics to FILE"
          , Option ['?'] ["help"]
                   (NoArg (\o -> o { showHelp = True }))
                   "Show usage information" ]

getOptions :: IO (Maybe Options)
getOptions =
    do args <- getArgs
       case args of
         [] -> return Nothing
         _  -> case getOpt (ReturnInOrder (\x o -> o { input = Just x })) options args of
                 (ox, n, []) ->
                     let o = foldl (flip id) defaultOptions ox in
                     case (showHelp o, input o, n) of
                       (True, _, _)    -> do putStrLn (usageInfo header options)
                                             exitSuccess
                       (_, Nothing, _) -> do hPutStrLn stderr "WARN: no input file specified on command line; disregarding other options"
                                             return Nothing
                       (_, _, _:_)     -> do hPutStrLn stderr ("WARN: ignoring unrecognized options " ++ intercalate ", " n)
                                             return (Just o)
                       _               -> return (Just o)
                 (_, _, errs) -> do hPutStrLn stderr (concat errs ++ usageInfo header options)
                                    exitFailure
    where header = "Usage: pdp8 [OPTIONS] object-file"


