import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Word
import Data.Binary.Get
import Data.ByteString.Lazy
import Text.Printf
import System.Environment
import System.IO
import Debug.Trace
import Data.Map

readFile :: Get [Word32]
readFile = do
  magic   <- getWord32le
  flags   <- getWord32le
  rec_count <- getWord32le -- discard for now
  address <- getWord32le
  size    <- getWord32le
  insns   <- replicateM (fromIntegral size) getWord32le
  return (insns)

loadState :: [Word32] -> Word32 -> State -> State
loadState [] _w s = s
loadState (x:xs) w s = loadState xs (w+1) (setMem w x s)

main :: IO ()
main = do argv <- getArgs
          let file = Prelude.head argv
          raw_data <- Data.ByteString.Lazy.readFile file
          let insns = runGet Main.readFile raw_data
          let inistate = (Data.Map.empty, Data.Map.empty)
          let loadstate = loadState insns 0x1000 inistate
          let first = toInstruction $ getMem 0x1000 loadstate
          let m = runCode first (setRegister P 0x1000 loadstate)
          System.IO.putStrLn (show m)
          return ()

data Operation = OP_BITWISE_OR          |
                 OP_BITWISE_AND         |
                 OP_ADD                 |
                 OP_MULTIPLY            |
                 OP_RESERVED0           |
                 OP_SHIFT_LEFT          |
                 OP_COMPARE_LT          |
                 OP_COMPARE_EQ          |
                 OP_COMPARE_GT          |
                 OP_BITWISE_ANDN        |
                 OP_BITWISE_XOR         |
                 OP_SUBTRACT            |
                 OP_BITWISE_XORN        |
                 OP_SHIFT_RIGHT_LOGICAL |
                 OP_COMPARE_NE          |
                 OP_RESERVED1 deriving Enum

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

data Register = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P deriving (Show, Enum, Eq, Ord)

data AddrMode = Mode00 | Mode01 | Mode10 | Mode11 deriving (Eq, Enum)

data Instruction = Illegal | Instruction { useImm :: Bool, addr :: AddrMode, op :: Operation, z :: Register, x :: Register, y :: Register, imm :: Word32}
instance Show Instruction where
  show Illegal = "illegal"
  show Instruction {useImm=False, addr=mode, op=oper, z=dst, x=src1, y=src2, imm=im} =
    case mode of
      Mode00 -> fmt "%s <- %s %s %s + 0x%08x"
      Mode01 -> fmt "%s <- [%s %s %s + 0x%08x]"
      Mode10 -> fmt "[%s] <- %s %s %s + 0x%08x"
      Mode11 -> fmt "%s -> [%s %s %s + 0x%08x]"
   where fmt s = printf s (show dst) (show src1) (show oper) (show src2) im
  show Instruction {useImm=True, addr=mode, op=oper, z=dst, x=src1, y=src2, imm=im} =
    case mode of
      Mode00 -> fmt "%s <- %s %s 0x%08x + %s"
      Mode01 -> fmt "%s <- [%s %s 0x%08x + %s]"
      Mode10 -> fmt "[%s] <- %s %s 0x%08x + %s"
      Mode11 -> fmt "%s -> [%s %s 0x%08x + %s]"
   where fmt s = printf s (show dst) (show src1) (show oper) im (show src2)

runCode :: Instruction -> State -> State
runCode Illegal state = state
runCode insn state = do let s1 = setRegister P (getRegister P state + 1) state
                        let s2 = evalInstruction s1 insn
                        let newInsn = toInstruction $ getMem (getRegister P s2) s2
                        runCode newInsn s2

showInstruction s [] = s
showInstruction s (x:xs) = do str <- fmap show x
                              System.IO.putStrLn str
                              showInstruction (liftM2 evalInstruction s x) xs

evalInstruction :: State -> Instruction -> State
evalInstruction s insn = evalInstruction' (addr insn)
    where oper = evalOp (op insn)
          dst  = getRegister (z insn) s
          src1 = getRegister (x insn) s
          src2 = getRegister (y insn) s
          im   = imm insn
          f    = if (useImm insn) then (oper src1 im src2) else (oper src1 src2 im)
          evalInstruction' Mode00 = setRegister (z insn) f s
          evalInstruction' Mode01 = setRegister (z insn) (getMem f s) s
          evalInstruction' Mode10 = setMem dst f s
          evalInstruction' Mode11 = setMem f dst s

type State    = (Map Register Word32, Map Word32 Word32)

getRegister :: Register -> State -> Word32
getRegister A   = \_ -> 0
getRegister reg = (findWithDefault 0 reg) . fst

setRegister :: Register -> Word32 -> State -> State
setRegister A _value state  = state
setRegister reg value state = (insert reg value (fst state), snd state)

getMem :: Word32 -> State -> Word32
getMem address = (findWithDefault 0 address) . snd

setMem :: Word32 -> Word32 -> State -> State
setMem address value state = (fst state, insert address value (snd state))

toInstruction :: Word32 -> Instruction
toInstruction 0xffffffff = Illegal
toInstruction w = Instruction useImm (toEnum mode) (toEnum oper) (toEnum dst) (toEnum src1) (toEnum src2) imm
        where useImm = (extract w 30 2) == 1
              mode   = fromIntegral $ extract w 28 2
              dst    = fromIntegral $ extract w 24 4
              src1   = fromIntegral $ extract w 20 4
              src2   = fromIntegral $ extract w 16 4
              oper   = fromIntegral $ extract w 12 4
              imm    = sex $ extract w 0 12

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

