{-# LANGUAGE RecordWildCards #-}
module Stats
  ( renderStats
  , renderLogs
  , renderMemoryLog
  , renderBranchLog
  , incStats
  ) where

import Arch
import Monad
import Parse
import Types

import Data.List (intercalate)
import Data.Maybe
import Numeric
import qualified Data.DList as D
import qualified Data.Map as M

renderLogs :: Stats -> String
renderLogs s =
  unlines [ "Memory Log:"
          , renderMemoryLog True (memoryLog s)
          , "Branch Log:"
          , renderBranchLog (branchLog s)
          ]

renderMemoryLog :: Bool -> MemoryLog -> String
renderMemoryLog descriptive
  = unlines
  . map (\(p,a) -> showB p ++ " " ++ show (unAddr a))
  . D.toList
  where
    showB InstrFetch | descriptive = "Instr Fetch"
                     | otherwise   = "2"
    showB DataRead   | descriptive = "Data Read  "
                     | otherwise   = "0"
    showB DataWrite  | descriptive = "Data Write "
                     | otherwise   = "1"

renderBranchLog :: BranchLog -> String
renderBranchLog
  = unlines
  . map (\(source,target,btype,taken) ->
             intercalate " "
                 [ showOct (unAddr source) ""
                 , showOct (unAddr target) ""
                 , case btype of
                     JMSBranch  -> "JMS"
                     JMPBranch  -> "JMP"
                     SkipBranch -> "SKP"
                 , if taken then "Taken" else "Not taken" ])
  . D.toList

renderStats :: Stats -> String
renderStats (Stats cy tot inst bl ml) =
     unlines [ "Total instructions: " ++ show tot
             , "Total cycles:       " ++ show cy
             , "Breakdown:        \n" ++ renderAll (M.toList inst)]
 where
  renderAll :: [(String,Integer)] -> String
  renderAll = unlines . map ("                " ++) . map renderLn
  renderLn :: (String,Integer) -> String
  renderLn (o,i) = o ++ " " ++ show i

-- |Increment the instruction AND cycle count
incStats :: Instr -> PDP8 ()
incStats i = modStats f
 where
   f (Stats cy tot bd x y) =
      let mp  = foldl incMnemonic bd (mnemonicOf i)
      in mp `seq` Stats (cy + nrCycles i) (tot+1) mp x y
   incMnemonic mp nic = M.insertWith (+) nic 1 mp

mnemonicOf :: Instr -> [String]
mnemonicOf (AND {}) = ["AND"]
mnemonicOf (TAD {}) = ["TAD"]
mnemonicOf (ISZ {}) = ["ISZ"]
mnemonicOf (DCA {}) = ["DCA"]
mnemonicOf (JMS {}) = ["JMS"]
mnemonicOf (JMP {}) = ["JMP"]
mnemonicOf (IOT i)  = [show i]
mnemonicOf (OP1 {..}) =
  let logicalL = maybeToList logical
      res = claStr cla (map show micros1) ++ map show logicalL
  in if null res then ["NOP"] else res
mnemonicOf (OP2 {..}) = claStr cla $ map show micros2 ++ showM skips
 where
  showM | invertAndUnion = \x ->
             let f y = case y of
                        SMA -> "SPA"
                        SZA -> "SNA"
                        SNL -> "SZL"
             in if null x then ["SKP"]
                          else map f x
        | otherwise      = map show
mnemonicOf (OP3 {..}) = claStr cla (map show micros3)
mnemonicOf (UNK {}) = ["UNK"]

claStr b = if b then ("CLA" :) else id
