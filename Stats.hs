module Stats
  ( renderStats
  ) where

renderStats :: Stats -> String
renderStats (Stats cy inst) =
  let nrInst = sum (M.elems inst)
  in "Total instructions: " ++ nrInst ++
     "Total cylces:       " ++ cy ++
     "Breakdown:          " ++ renderAll (M.toList inst)
 where
  renderAll :: [(PDPOp,Integer)] -> String
  renderall = unlines. map ("                 " ++) . map renderLn
  renderLn :: (PDPOp,Integer) -> String
  renderLn (o,i) = show o ++ " " ++ show i
