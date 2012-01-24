module Stats
  ( renderStats
  ) where
import Parse
import Types
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
