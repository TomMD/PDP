module Arch where

nrCyclesOp :: PDPOp -> Int
nrCyclesOp o
  | op >= 0 && <= 4 = 2
  | op == 5         = 1
  | op == 6         = 0
  | otherwise       = 1
 where
 op = fromEnum o

nrCycles :: Instr -> Int
nrCycles = error "TODO"
