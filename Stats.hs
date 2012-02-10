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
  unlines [ renderMemoryLog (memoryLog s)
          , renderBranchLog (branchLog s)
          ]

renderMemoryLog :: MemoryLog -> String
renderMemoryLog
  = unlines
  . map (\(p,a) -> show (fromEnum p) ++ " " ++ showOct (unAddr a) "")

renderBranchLog :: BranchLog -> String
renderBranchLog
  = unlines
  . map (\(p,a) -> show p ++ " " ++ showOct (unAddr a) "")

renderStats :: Stats -> String
renderStats (Stats cy inst bl ml) =
  let nrInst = sum (M.elems inst)
  in unlines [ "Total instructions: " ++ show nrInst
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
   f (Stats cy bd x y) = Stats (cy + nrCycles i) (foldl incMnemonic bd (mnemonicOf i)) x y
   incMnemonic mp nic = M.insertWith (+) nic 1 mp
   
mnemonicOf :: Instr -> [String]
mnemonicOf (AND {}) = ["AND"]
mnemonicOf (TAD {}) = ["TAD"]
mnemonicOf (ISZ {}) = ["ISZ"]
mnemonicOf (DCA {}) = ["DCA"]
mnemonicOf (JMS {}) = ["JMS"]
mnemonicOf (JMP {}) = ["JMP"]
mnemonicOf (IOT i)  = [show i]
mnemonicOf (OP1 {..}) = map show micros1
mnemonicOf (OP2 {..}) = map show micros2
mnemonicOf (OP3 {..}) = map show micros3
mnemonicOf (UNK {}) = ["UNK"]
