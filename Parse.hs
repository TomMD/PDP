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
  ) where

import Numeric (readOct)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Char (digitToInt, isDigit)

data Value = VInstr Instr | VAddr Addr
  deriving (Eq, Ord, Show)

type Offset = Int

-- The 'Instr'uncation data type carries the decoded op,
-- indirection, memory page flags, and offset if applicable,
-- and the raw integer that was decoded.
data Instr = Instr PDPOp (Maybe Indirection) (Maybe MemPage) (Maybe Offset) Int
  deriving (Eq, Ord, Show)

instr x raw = Instr x Nothing Nothing Nothing raw

type PDPOps = [PDPOp]
data PDPOp
  = AND  -- Logical and
  | TAD  -- two's comp add
  | ISZ  -- Increment memory and inc PC if zero
  | DCA  -- store acc to memory and clear acc
  | JMS  -- store PC to addr, read new pc from addr + 1
  | JMP  -- read new pc from addr
  -- A general placeholder for IO
  | IOOp
  -- Microcodes
  | NOP
  | CLA1
  | CLL
  | CMA
  | CML
  | IAC
  | RAR
  | RTR
  | RAL
  | RTL
  -- Group 2
  | SMA
  | SZA
  | SNL
  | SPA
  | SNA
  | SZL
  | SKP
  | CLA2
  | OSR
  | HLT
  -- Group 3
  | CLA3
  | MQL
  | MQA
  | SWP
  | CAM
  | UnknownMicroOp
  | UnknownOp
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

decodeInstr :: Int -> Instr
decodeInstr i
  | op >= 0 && op < 6 = decodeMemInstr (fromEnum op) i
  | op == 6           = instr IOOp i
  | op == 7           = instr (decodeMicroInstr i) i
  | otherwise         = UnknownOp
 where
  op = i `shiftR` 9

decodeMemInstr :: Int -> Int -> Instr
decodeMemInstr op i = Instr (toEnum op) (Just ind) (Just mempg) (Just off) i
 where
 ind   = if testBit i 8 then Indirect else Direct
 mempg = if testBit i 7 then CurrentPage else ZeroPage
 off   = i .&. ob 1111111

-- Micro instrs
decodeMicroInstr :: Int -> PDPOp
-- Group 1
decodeMicroInstr 3584 = NOP
decodeMicroInstr 3712 = CLA1
decodeMicroInstr 3648 = CLL
decodeMicroInstr 3616 = CMA
decodeMicroInstr 3600 = CML
decodeMicroInstr 3585 = IAC
decodeMicroInstr 3592 = RAR
decodeMicroInstr 3594 = RTR
decodeMicroInstr 3588 = RAL
decodeMicroInstr 3590 = RTL
-- Group 2 micro instrs
decodeMicroInstr 3904 = SMA
decodeMicroInstr 3872 = SZA
decodeMicroInstr 3856 = SNL
decodeMicroInstr 3912 = SPA
decodeMicroInstr 3880 = SNA
decodeMicroInstr 3864 = SZL
decodeMicroInstr 3848 = SKP
decodeMicroInstr 3968 = CLA2
decodeMicroInstr 3844 = OSR
decodeMicroInstr 3842 = HLT
-- Group 3 micro instrs
decodeMicroInstr 3969 = CLA3
decodeMicroInstr 3857 = MQL
decodeMicroInstr 3905 = MQA
decodeMicroInstr 3921 = SWP
decodeMicroInstr 3985 = CAM
decodeMicroInstr _ = UnknownMicroOp

-- "To Binary" intented to appear like
-- 0b11011 in the same way 0x.... is hex
ob :: Integer -> Int
ob = foldl (\s x -> shiftL s 1 .|. x) 0
   . map digitToInt
   . show
