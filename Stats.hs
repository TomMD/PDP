{-# LANGUAGE RecordWildCards #-}
module Stats
  ( renderStats
  , renderLogs
  , renderMemoryLog
  , renderBranchLog
  , incStats
  ) where

import Arch
import Parse
import Types
import Monad

import qualified Data.Map as M
import Numeric

renderLogs :: Stats -> String
renderLogs s =
  unlines [ "Memory Log:"
          , renderMemoryLog (memoryLog s)
          , "Branch Log:"
          , renderBranchLog (branchLog s)
          ]

renderMemoryLog :: MemoryLog -> String
renderMemoryLog
  = unlines
  . map (\(p,a) -> showB p ++ " " ++ showOct (unAddr a) "")
  where
    showB InstrFetch = "Instr Fetch"
    showB DataRead   = "Data Read  "
    showB DataWrite  = "Data Write "

renderBranchLog :: BranchLog -> String
renderBranchLog
  = unlines
  . map (\(p,a) -> show p ++ " " ++ showOct (unAddr a) "")

renderStats :: Stats -> String
renderStats (Stats cy tot inst bl ml) =
     unlines [ "Total instructions: " ++ show tot
             , "Total cycles:       " ++ show cy
             , "Breakdown:        \n" ++ renderAll (M.toList inst)]
 where
  renderAll :: [(String,Integer)] -> String
  renderAll = unlines . map ("                 " ++) . map renderLn
  renderLn :: (String,Integer) -> String
  renderLn (o,i) = o ++ " " ++ show i

-- |Increment the instruction AND cylce count
incStats :: Instr -> PDP8 ()
incStats i = modStats f
 where
   f (Stats cy tot bd x y) = Stats (cy + nrCycles i) (tot+1) (foldl incMnemonic bd (mnemonicOf i)) x y
   incMnemonic mp nic = M.insertWith (+) nic 1 mp

mnemonicOf :: Instr -> [String]
mnemonicOf (AND {}) = ["AND"]
mnemonicOf (TAD {}) = ["TAD"]
mnemonicOf (ISZ {}) = ["ISZ"]
mnemonicOf (DCA {}) = ["DCA"]
mnemonicOf (JMS {}) = ["JMS"]
mnemonicOf (JMP {}) = ["JMP"]
mnemonicOf (IOT i)  = [show i]
mnemonicOf (OP1 {..}) = (if cla then ("CLA" :) else id) (map show micros1)
mnemonicOf (OP2 {..}) = map show micros2
mnemonicOf (OP3 {..}) = (if cla then ("CLA" :) else id) (map show micros3)
mnemonicOf (UNK {}) = ["UNK"]
