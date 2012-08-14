import Data.Bits
import Data.Word
import Text.Printf
import System.Environment

main :: IO ()
main = do argv <- getArgs
          let file = head argv
          putStrLn file

data Operation = OP_BITWISE_OR          |
                 OP_BITWISE_AND         |
                 OP_ADD                 |
                 OP_MULTIPLY            |
                 OP_SHIFT_LEFT          |
                 OP_COMPARE_LT          |
                 OP_COMPARE_EQ          |
                 OP_COMPARE_GT          |
                 OP_BITWISE_ANDN        |
                 OP_BITWISE_XOR         |
                 OP_SUBTRACT            |
                 OP_BITWISE_XORN        |
                 OP_SHIFT_RIGHT_LOGICAL |
                 OP_COMPARE_NE deriving Enum

instance Show Operation where
  show oper =
    case oper of
      OP_BITWISE_OR          -> "|"
      OP_BITWISE_AND         -> "&"
      OP_ADD                 -> "+"
      OP_MULTIPLY            -> "*"
      OP_SHIFT_LEFT          -> "<<"
      OP_COMPARE_LT          -> "<"
      OP_COMPARE_EQ          -> "=="
      OP_COMPARE_GT          -> ">"
      OP_BITWISE_ANDN        -> "&~"
      OP_BITWISE_XOR         -> "^"
      OP_SUBTRACT            -> "-"
      OP_BITWISE_XORN        -> "^~"
      OP_SHIFT_RIGHT_LOGICAL -> ">>"
      OP_COMPARE_NE          -> "/="

data Register = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P deriving (Show, Enum)

data AddrMode = Mode00 | Mode01 | Mode10 | Mode11 deriving (Eq, Enum)

data Instruction = Instruction { addr :: AddrMode, op :: Operation, z :: Register, x :: Register, y :: Register, imm :: Word32}
instance Show Instruction where
  show Instruction {addr=mode, op=oper, z=dst, x=src1, y=src2, imm=im} =
    case mode of
      Mode00 -> fmt "%s <- %s %s %s + 0x%08x"
      Mode01 -> fmt "%s <- [%s %s %s + 0x%08x]"
      Mode10 -> fmt "[%s] <- %s %s %s + 0x%08x"
      Mode11 -> fmt "%s -> [%s %s %s + 0x%08x]"
   where fmt s = printf s (show dst) (show src1) (show oper) (show src2) im

toInstruction :: Word32 -> Instruction
toInstruction w = Instruction (toEnum mode) (toEnum oper) (toEnum dst) (toEnum src1) (toEnum src2) imm
  where mode = fromIntegral $ extract w 28 2
        dst  = fromIntegral $ extract w 24 4
        src1 = fromIntegral $ extract w 20 4
        src2 = fromIntegral $ extract w 16 4
        oper = fromIntegral $ extract w 12 4
        imm  = sex $ extract w 0 12

data State = State { a :: Word32, b :: Word32,
                     c :: Word32, d :: Word32,
                     e :: Word32, f :: Word32,
                     g :: Word32, h :: Word32,
                     i :: Word32, l :: Word32,
                     m :: Word32, n :: Word32,
                     o :: Word32, p :: Word32
                   }

extract :: Word32 -> Int -> Int -> Word32
extract word start len = (shiftR word start) .&. (shiftL 1 len - 1)

-- Sign EXtension
sex :: Word32 -> Word32
sex n = n .|. (0xfffff800 * ((shiftR n 11) .&. 1))

evalOp :: Operation -> Word32 -> Word32 -> Word32 -> Word32
evalOp f x y i = (getOp f) x y + i

boolToReg :: Bool -> Word32
boolToReg False = 0
boolToReg True  = 0xffffffff

-- don't ask
foo :: Word32 -> Word32
foo = xor 0x80000000

getOp :: Operation -> (Word32 -> Word32 -> Word32)
getOp oper =
  case oper of
    OP_BITWISE_OR          -> \x y -> x .|. y
    OP_BITWISE_AND         -> \x y -> x .&. y
    OP_ADD                 -> \x y -> x + y
    OP_MULTIPLY            -> \x y -> x * y
    OP_SHIFT_LEFT          -> \x y -> x * (2 ^ y)
    OP_COMPARE_LT          -> \x y -> boolToReg (foo x < foo y)
    OP_COMPARE_EQ          -> \x y -> boolToReg (x == y)
    OP_COMPARE_GT          -> \x y -> boolToReg (foo x > foo y)
    OP_BITWISE_ANDN        -> \x y -> x .&. (complement y)
    OP_BITWISE_XOR         -> \x y -> x `xor` y
    OP_SUBTRACT            -> \x y -> x - y
    OP_BITWISE_XORN        -> \x y -> x `xor` (complement y)
    OP_SHIFT_RIGHT_LOGICAL -> \x y -> x `div` (2 ^ y)
    OP_COMPARE_NE          -> \x y -> boolToReg (x /= y)

