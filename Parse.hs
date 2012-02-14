module Parse
  ( -- * Types
    Value(..)
  , Instr(..)
  , Offset
  , Indirection(..)
  , MemPage(..)
  , Addr(..)
  -- * The functions
  , parseObj
  , decodeInstr
  ) where

import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Char (digitToInt, isDigit)
import Numeric

import Types
import Util

isInstr :: Value -> Bool
isInstr (VInstr _) = True
isInstr _          = False

isAddr :: Value -> Bool
isAddr = not . isInstr

-- Parse and ASCii obj file into an AST
parseObj :: String -> [Value]
parseObj = lexer . map (filter isDigit) . lines

-- The lexer accepts strings of 3 digit octel numbers in pairs of two
-- and produces "Values" in the form of memory addresses or instructions.
lexer :: [String] -> [Value]
lexer [] = []
lexer [l] = error $ "PDP8 object format should have an even number of lines, \
                    \but there exists a lone trailing line of: " ++ l
lexer (a:b:is) = lexx a b : lexer is

-- Lexx is the most powerful destructive weapon in the two universes.
lexx :: String -> String -> Value
lexx [] _ = error "Lexing expected a six digit octal number, but found an empty string."
lexx (f:a) b
  | isAddrFormat f = VAddr  (Addr i)
  | otherwise      = VInstr (decodeInstr i, i)
 where
   readOct' :: String -> Int12
   readOct' = fst . head. readOct
   i = readOct' . (a ++) . drop 1 $ b

isAddrFormat '1' = True
isAddrFormat '0' = False
isAddrFormat c   = error ("Invalid initial character in an OBJ line: " ++ show c)

decodeInstr :: Int12 -> Instr
decodeInstr i
  | op >= 0 && op < 6            = decodeMemInstr op i
  | op == 6                      = decodeIOInstr i
  | op == 7 && not (testBit i 8) = case decodeMicroInstr1 i of
                                     Just i  -> i
                                     Nothing -> UNK i
  | op == 7 && testBit i 8
            && not (testBit i 0) = decodeMicroInstr2 i
  | op == 7 && testBit i 8
            && testBit i 0       = decodeMicroInstr3 i
  | otherwise                    = UNK i
 where
  op = ob 111 .&. (i `shiftR` 9)

decodeMemInstr :: Int12 -> Int12 -> Instr
decodeMemInstr op i = opCon op ind mempg off
 where
 opCon 0 = AND
 opCon 1 = TAD
 opCon 2 = ISZ
 opCon 3 = DCA
 opCon 4 = JMS
 opCon 5 = JMP
 opCon _ = \_ _ _ -> UNK i

 ind   = if testBit i 8 then Indirect else Direct
 mempg = if testBit i 7 then CurrentPage else ZeroPage
 off   = i .&. ob 1111111

decodeMicro :: [(Int,a)] -> Int12 -> (Bool, [a])
decodeMicro masks i = ( testBit i 7
                      , map snd (filter (\(bit, _) -> testBit i bit) masks) )

decodeIOInstr :: Int12 -> Instr
decodeIOInstr i
  = case op of
         Just o  -> IOT o
         Nothing -> UNK i
 where
 op
  | i == oct 6030 = Just KCF
  | i == oct 6031 = Just KSF
  | i == oct 6032 = Just KCC
  | i == oct 6034 = Just KRS
  | i == oct 6036 = Just KRB
  | i == oct 6040 = Just TFL
  | i == oct 6041 = Just TSF
  | i == oct 6042 = Just TCF
  | i == oct 6044 = Just TPC
  | i == oct 6046 = Just TLS
  | otherwise     = Nothing

decodeMicroInstr1 :: Int12 -> Maybe Instr
decodeMicroInstr1 i =
    case (testBit i 3, testBit i 2, testBit i 1) of
      (True, False, b) ->
          Just (OP1 cla ops (if b then Just RTR else Just RAR))
      (False, True, b) ->
          Just (OP1 cla ops (if b then Just RTL else Just RAL))
      (False, False, b) ->
          Just (OP1 cla ops (if b then Just BSW else Nothing))
      _ ->
          Nothing
    where (cla, ops) = decodeMicro microOp1Masks i

decodeMicroInstr2 :: Int12 -> Instr
decodeMicroInstr2 i = OP2 cla (testBit i 3) skips micros
    where (cla, skips) = decodeMicro skipOpMasks i
          (_, micros) = decodeMicro microOp2Masks i

decodeMicroInstr3 :: Int12 -> Instr
decodeMicroInstr3 i | MQL `elem` ops && MQA `elem` ops = UNK i
                    | otherwise                        = OP3 cla ops
    where (cla, ops) = decodeMicro microOp3Masks i

microOp1Masks = [(6, CLL), (5, CMA), (4, CML), (0, IAC)]
skipOpMasks = [(6, SMA), (5, SZA), (4, SNL)]
microOp2Masks = [(2, OSR), (1, HLT)]
microOp3Masks = [(7, MQA), (5, MQL)]
