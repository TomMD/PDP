module Execute
       ( step
       ) where

import Control.Monad
import Data.Bits

import Monad
import Stats
import Types
import Memory
import Util

step :: PDP8 ()
step = do iaddr <- getPC
          modPC (1+)
          i <- fetchInstruction (Addr iaddr)
          case i of
            UNK {} -> return () -- TODO: log
            OP1 cla micros logical -> doOp1 cla micros logical
            OP2 cla invertAndUnion skips micros -> doOp2 cla invertAndUnion skips micros
            OP3 cla micros -> doOp3 cla micros
            IOT ioop -> doIO ioop
            _ -> execute i =<< effectiveAddr i iaddr

doOp1 cla micros logical =
    do when cla (setAC 0)
       when (CLL `elem` micros) (setL 0)
       when (CMA `elem` micros) (modAC complement)
       when (CML `elem` micros) (modL complement)
       when (IAC `elem` micros) (modAC (+1))
       case logical of
         Nothing -> return ()
         Just RAR -> modLAC rotR
         Just RTR -> modLAC (rotR . rotR)
         Just RAL -> modLAC rotL
         Just RTL -> modLAC (rotL . rotL)
         Just BSW -> modAC (\a -> let high = (a .&. oct 7700) `shiftR` 6
                                      low  = (a .&. oct 0077) `shiftL` 6
                                  in low .|. high)
    where rotL, rotR :: (Int,Int12) -> (Int,Int12)
          rotL (l, a) = (chiNeq (a .&. 4000) 0, a `shiftL` 1 .|. fromIntegral l)
          rotR (l, a) = (chiNeq (a .&. 1) 0, fromIntegral (l `shiftL` 11) .|. a `shiftR` 1)
          chiNeq x y | x /= y    = 1
                     | otherwise = 0

doOp2 cla invertAndUnion skips micros =
    do logic invertAndUnion skips
       when cla (setAC 0)
       when (OSR `elem` micros) (modAC . (.|.) =<< getSR)
       when (HLT `elem` micros) halt
    where logic True []     = skipIf True
          logic False skips = skipIf . or =<< mapM check skips
          logic True skips  = skipIf . and . map not =<< mapM check skips

          check SMA = (< 0) `fmap` getAC
          check SZA = (== 0) `fmap` getAC
          check SNL = (/= 0) `fmap` getL

          skipIf True  = modPC (1 +)
          skipIf False = return ()

doOp3 cla micros =
    do when cla (setAC 0)
       when (MQL `elem` micros) (do setMQ =<< getAC
                                    setAC 0)
       when (MQA `elem` micros) (modAC . (.|.) =<< getMQ)

doIO _ = return ()

execute i a@(Addr v) =
    do operand <- load a
       case i of
         AND {} -> modAC (operand .&.)
         TAD {} -> do a <- getAC
                      let sum = a + operand
                      setAC sum
                      when (signum a == signum operand && signum a /= signum sum) (modL complement)
         ISZ {} -> let operand' = operand + 1
                   in do store a operand'
                         when (operand' == 0) (modPC (1+))
         DCA {} -> do store a =<< getAC
                      setAC 0
         JMS {} -> do store a =<< getPC
                      setPC (v + 1)
         JMP {} -> setPC v
