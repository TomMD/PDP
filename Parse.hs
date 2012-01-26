module Parse 
  ( -- * Types
    Value(..)
  , Instr(..)
  , PDPOps, Offset
  , PDPOp(..)
  , Indirection(..)
  , MemPage(..)
  , Addr(..)
  -- * The functions
  , parseObj
  , decodeInstr
  , encodeInstr
  ) where

import Numeric (readOct)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Char (digitToInt, isDigit)

data Value = VInstr { vInstr :: Instr } | VAddr { vAddr :: Addr }
  deriving (Eq, Ord, Show)

isInstr :: Value -> Bool
isInstr (VInstr _) = True
isInstr _          = False

isAddr :: Value -> Bool
isAddr = not . isInstr

type Offset = Int

-- The 'Instr'uncation data type carries the decoded op,
-- indirection, memory page flags, and offset if applicable,
-- and the raw integer that was decoded.
data Instr = Instr 
  { instrOp          :: PDPOp
  , instrIndirection :: (Maybe Indirection)
  , instrMemPage     :: (Maybe MemPage)
  , instrOffset      :: (Maybe Offset)
  , instrCode        :: Int }
  deriving (Eq, Ord, Show)

instr constr [] raw = Instr UnknownOp   Nothing Nothing Nothing raw
instr constr xs raw = Instr (constr xs) Nothing Nothing Nothing raw

type PDPOps = [PDPOp]
data PDPOp = PDPOpMem MemOp
           | PDPOpIO IOOp 
           | PDPOpMicro1 [MicroOp1]
           | PDPOpMicro2 [MicroOp2]
           | PDPOpMicro3 [MicroOp3]
           | UnknownOp
  deriving (Eq, Ord, Show)

data MemOp  
  = AND  -- Logical and
  | TAD  -- two's comp add
  | ISZ  -- Increment memory and inc PC if zero
  | DCA  -- store acc to memory and clear acc
  | JMS  -- store PC to addr, read new pc from addr + 1
  | JMP  -- read new pc from addr
  deriving (Eq, Ord, Show, Enum)

data IOOp =
  -- A general placeholder for IO
    IOOp
  deriving (Eq, Ord, Show, Enum)

data MicroOp1 =
  -- Microcodes
    NOP
  | CLA1
  | CLL
  | CMA
  | CML
  | IAC
  | RAR
  | RTR
  | RAL
  | RTL
  deriving (Eq, Ord, Show, Enum)

data MicroOp2 =
  -- Group 2
    SMA
  | SZA
  | SNL
  | SPA
  | SNA
  | SZL
  | SKP
  | CLA2
  | OSR
  | HLT
  deriving (Eq, Ord, Show, Enum)

data MicroOp3 =
  -- Group 3
    CLA3
  | MQL
  | MQA
  | SWP
  | CAM
  deriving (Eq, Ord, Show, Enum)

data Indirection = Direct | Indirect
  deriving (Eq, Ord, Show)
data MemPage = ZeroPage | CurrentPage
  deriving (Eq, Ord, Show)

newtype Addr = Addr { unAddr :: Int }
  deriving (Eq, Ord, Show)

-- |Parse and ASCii obj file into an AST
parseObj :: String -> [Value]
parseObj = lexer . map (filter isDigit) . lines

-- The lexer accepts strings of 3 digit octel numbers in pairs of two
-- and produces "Values" in the form of memory addresses or instructions.
lexer :: [String] -> [Value]
lexer [] = []
lexer [l] = error $ "PDP8 object format should have an even number of lines, but there exists a lone trailing line of: " ++ l
lexer (a:b:is) = lexx a b : lexer is

lexx :: String -> String -> Value
lexx (f:a) b 
  | isAddrFormat f = VAddr  . Addr        . readOct' . (a ++) . drop 1 $ b
  | otherwise      = VInstr . decodeInstr . readOct' . (a ++) . drop 1 $ b
 where
  readOct' = fst . head. readOct

isAddrFormat '1' = True
isAddrFormat '0' = False
isAddrFormat c   = error ("Invalid initial character in an OBJ line: " ++ show c)

encodeInstr :: Instr -> Int
encodeInstr (Instr _ _ _ _ i) = i

decodeInstr :: Int -> Instr
decodeInstr i
  | op >= 0 && op < 6 = decodeMemInstr op i
  | op == 6           = Instr (PDPOpIO IOOp) Nothing Nothing Nothing i
  | op == 7 && not (testBit i 8) = instr PDPOpMicro1 (decodeMicroInstr1 i) i
  | op == 7 && testBit i 8
            && testBit i 0       = instr PDPOpMicro2 (decodeMicroInstr2 i) i
  | op == 7 && testBit i 8
            && testBit i 0       = instr PDPOpMicro3 (decodeMicroInstr3 i) i
  | otherwise         = instr (const UnknownOp) [] i
 where
  op = i `shiftR` 9

decodeMemInstr :: Int -> Int -> Instr
decodeMemInstr op i = Instr (PDPOpMem $ toEnum op) (Just ind) (Just mempg) (Just off) i
 where
 ind   = if testBit i 8 then Indirect else Direct
 mempg = if testBit i 7 then CurrentPage else ZeroPage
 off   = i .&. ob 1111111

decodeMicro :: [(Int,a)] -> Int -> [a]
decodeMicro masks i
  = map snd
  . filter ((\code -> (i .&. code) == code) . fst)
  $ masks

-- Micro instrs
decodeMicroInstr1 :: Int -> [MicroOp1]
decodeMicroInstr1 = decodeMicro microOp1Masks

decodeMicroInstr2 :: Int -> [MicroOp2]
decodeMicroInstr2 = decodeMicro microOp2Masks

decodeMicroInstr3 :: Int -> [MicroOp3]
decodeMicroInstr3 = decodeMicro microOp3Masks

-- Group 1
microOp1Masks :: [(Int,MicroOp1)]
microOp1Masks =
  [(oct 7000, NOP), (oct 7200, CLA1), (oct 7100, CLL), (oct 7040, CMA), (oct 7020, CML),
   (oct 7001, IAC), (oct 7010, RAR), (oct 7012, RTR), (oct 7004, RAL), (oct 7006, RTL)]

-- Group 2 micro instrs
microOp2Masks :: [(Int,MicroOp2)]
microOp2Masks =
  [(oct 7500, SMA), (oct 7440, SZA), (oct 7420, SNL), (oct 7510, SPA), (oct 7450, SNA),
   (oct 7430, SZL), (oct 7410, SKP), (oct 7600, CLA2), (oct 7404, OSR), (oct 7402, HLT)]

-- Group 3 micro instrs
microOp3Masks :: [(Int,MicroOp3)]
microOp3Masks =
  [(oct 7601, CLA3), (oct 7421, MQL), (oct 7501, MQA), (oct 7521, SWP), (oct 7621, CAM)]

-- "To Binary" intented to appear like
-- 0b11011 in the same way 0x.... is hex
ob :: Integer -> Int
ob = foldl (\s x -> shiftL s 1 .|. x) 0
   . map digitToInt
   . show
