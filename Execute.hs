module Execute
       ( step
       ) where

import Control.Monad
import Data.Bits

import Monad
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
            OP2 cla invertAndUnion skips micros -> doOp2 iaddr cla invertAndUnion skips micros
            OP3 cla micros -> doOp3 cla micros
            IOT ioop -> doIO iaddr ioop
            _ -> execute iaddr i =<< effectiveAddr i iaddr

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
          rotL (l, a) = (chiNeq (a .&. oct 4000) 0, a `shiftL` 1 .|. fromIntegral l)
          rotR (l, a) = (chiNeq (a .&. 1) 0, fromIntegral (l `shiftL` 11) .|. a `shiftR` 1)
          chiNeq x y | x /= y    = 1
                     | otherwise = 0

skipIf iaddr True  = logSkip iaddr True >> modPC (1 +)
skipIf iaddr False = logSkip iaddr False >> return ()

logSkip iaddr = logBranch (Addr iaddr) (Addr (iaddr + 2)) SkipBranch

doOp2 iaddr cla invertAndUnion skips micros =
    do logic invertAndUnion skips
       when cla (setAC 0)
       when (OSR `elem` micros) (modAC . (.|.) =<< getSR)
       when (HLT `elem` micros) halt
    where logic True []     = skipIf iaddr True
          logic False skips = skipIf iaddr . or =<< mapM check skips
          logic True skips  = skipIf iaddr . and . map not =<< mapM check skips

          check SMA = (< 0) `fmap` getAC
          check SZA = (== 0) `fmap` getAC
          check SNL = (/= 0) `fmap` getL

doOp3 cla micros =
    do when cla (setAC 0)
       when (MQL `elem` micros) (do setMQ =<< getAC
                                    setAC 0)
       when (MQA `elem` micros) (modAC . (.|.) =<< getMQ)

doIO :: Int12 -> IOOp -> PDP8 ()
doIO iaddr KCF = setKeyboardFlag False
doIO iaddr KSF = getKeyboardFlag >>= skipIf iaddr
doIO iaddr KCC = setKeyboardFlag False >> setAC 0
doIO iaddr KRS = getKB >>= \kb -> modAC (.|. kb)
doIO iaddr KRB = setKeyboardFlag False >> getKB >>= setAC
doIO iaddr TFL = return () -- error "TFL is a PDP-8/E only instruction"
doIO iaddr TSF = getTeleprinterFlag >>= skipIf iaddr
doIO iaddr TCF = setTeleprinterFlag False
doIO iaddr TPC = outputStr . (\c -> [c]) . toEnum . fromIntegral . (.&. 0x7F) =<< getAC
doIO iaddr TLS = doIO iaddr TPC >> setTeleprinterFlag False

execute iaddr i addr@(Addr v) = do
       case i of
         AND {} -> load addr >>= \operand -> modAC (operand .&.)
         TAD {} -> do operand <- load addr
                      a <- getAC
                      let sum = a + operand
                      setAC sum
                      when (signum a == signum operand && signum a /= signum sum) (modL complement)
         ISZ {} -> do operand <- fmap (+1) (load addr)
                      store addr operand
                      when (operand == 0) (modPC (1+))
         DCA {} -> do store addr =<< getAC
                      setAC 0
         JMS {} -> do store addr (iaddr + 1)
                      setPC (v + 1)
                      logBranch (Addr iaddr) (Addr (v + 1)) JMSBranch True
         JMP {} -> do logBranch (Addr iaddr) (Addr v) JMPBranch True
                      setPC v
         _      -> return ()
