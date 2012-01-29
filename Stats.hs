module Stats
  ( renderStats
  , incStats
  ) where

import Arch
import Parse
import Types
import Monad
import qualified Data.Map as M

renderStats :: Stats -> String
renderStats (Stats cy inst) =
  let nrInst = sum (M.elems inst)
  in "Total instructions: " ++ show nrInst ++
     "Total cycles:       " ++ show cy ++
     "Breakdown:          " ++ renderAll (M.toList inst)
 where
  renderAll :: [(PDPOp,Integer)] -> String
  renderAll = unlines. map ("                 " ++) . map renderLn
  renderLn :: (PDPOp,Integer) -> String
  renderLn (o,i) = show o ++ " " ++ show i

-- |Increment the instruction AND cylce count
incStats :: Instr -> PDP8 ()
incStats i@(Instr op _ _ _ _) = modStats f
 where
   f (Stats cy bd) = Stats (cy + nrCycles i) (M.insertWith (+) op 1 bd)