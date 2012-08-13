import Data.Bits
import Data.Word
import Text.Printf

data Operation = OP_BITWISE_OR          |
                 OP_BITWISE_AND         |
                 OP_ADD                 |
                 OP_MULTIPLY            |
                 OP_SHIFT_LEFT          |
                 OP_COMPARE_LT          |
                 OP_COMPARE_EQ          |
                 OP_COMPARE_GT          |
                 OP_BITWISE_NAND        |
                 OP_BITWISE_XOR         |
                 OP_ADD_NEGATIVE_Y      |
                 OP_XOR_INVERT_Y        |
                 OP_SHIFT_RIGHT_LOGICAL |
                 OP_COMPARE_NE

instance Show Operation where
  show OP_BITWISE_OR          = "|"
  show OP_BITWISE_AND         = "&"
  show OP_ADD                 = "+"
  show OP_MULTIPLY            = "*"
  show OP_SHIFT_LEFT          = "<<"
  show OP_COMPARE_LT          = "<"
  show OP_COMPARE_EQ          = "=="
  show OP_COMPARE_GT          = ">"
  show OP_BITWISE_NAND        = "&~"
  show OP_BITWISE_XOR         = "^"
  show OP_ADD_NEGATIVE_Y      = "-"
  show OP_XOR_INVERT_Y        = "^~"
  show OP_SHIFT_RIGHT_LOGICAL = ">>"
  show OP_COMPARE_NE          = "/="

data Register = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P deriving Show

data AddrMode = Mode00 | Mode01 | Mode10 | Mode11 deriving Eq

data Instruction = Instruction { addr :: AddrMode, op :: Operation, z :: Register, x :: Register, y :: Register, imm :: Word32}
instance Show Instruction where
  show Instruction {addr=mode, op=oper, z=dst, x=src1, y=src2, imm=im} =
    case mode of
      Mode00 -> fmt "%s <- %s %s %s + %u"
      Mode01 -> fmt "%s <- [%s %s %s + %u]"
      Mode10 -> fmt "[%s] <- %s %s %s + %u"
      Mode11 -> fmt "%s -> [%s %s %s + %u]"
   where fmt s = printf s (show dst) (show src1) (show oper) (show src2) im

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

evalOp :: Operation -> Word32 -> Word32 -> Word32 -> Word32
evalOp f x y i = (getOp f) x y + i

boolToReg :: Bool -> Word32
boolToReg False = 0
boolToReg True  = 0xffffffff

decodeRegister :: Int -> Register
decodeRegister = ([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P] !!)

-- don't ask
foo :: Word32 -> Word32
foo = xor 0x80000000

decodeOp :: Int -> Operation
decodeOp = ([OP_BITWISE_OR, OP_BITWISE_AND, OP_ADD, OP_MULTIPLY, OP_SHIFT_LEFT,
        OP_COMPARE_LT, OP_COMPARE_EQ, OP_COMPARE_GT, OP_BITWISE_NAND, OP_ADD_NEGATIVE_Y,
        OP_XOR_INVERT_Y, OP_SHIFT_RIGHT_LOGICAL, OP_COMPARE_NE] !!)

getOp :: Operation -> (Word32 -> Word32 -> Word32)
getOp OP_BITWISE_OR          = \x y -> x .|. y
getOp OP_BITWISE_AND         = \x y -> x .&. y
getOp OP_ADD                 = \x y -> x + y
getOp OP_MULTIPLY            = \x y -> x * y
getOp OP_SHIFT_LEFT          = \x y -> x * (2 ^ y)
getOp OP_COMPARE_LT          = \x y -> boolToReg (foo x < foo y)
getOp OP_COMPARE_EQ          = \x y -> boolToReg (x == y)
getOp OP_COMPARE_GT          = \x y -> boolToReg (foo x > foo y)
getOp OP_BITWISE_NAND        = \x y -> complement (x .&. y)
getOp OP_BITWISE_XOR         = \x y -> x `xor` y
getOp OP_ADD_NEGATIVE_Y      = \x y -> x - y
getOp OP_XOR_INVERT_Y        = \x y -> x `xor` (complement y)
getOp OP_SHIFT_RIGHT_LOGICAL = \x y -> x `div` (2 ^ y)
getOp OP_COMPARE_NE          = \x y -> boolToReg (x /= y)

