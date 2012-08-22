import Control.Monad
import Data.Bits
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import System.Environment
import System.IO
import Data.Map

data Operation = OP_BIT_OR    | OP_BIT_AND  | OP_ADD     | OP_MUL
               | OP_RESERVED0 | OP_SHIFTL   | OP_LT      | OP_EQ
               | OP_GT        | OP_BIT_ANDN | OP_BIT_XOR | OP_SUB
               | OP_BIT_XORN  | OP_SHIFTR   | OP_NEQ     | OP_RESERVED1
  deriving Enum

data Register = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P deriving (Enum, Eq, Ord, Show)

data AddrMode = Mode00 | Mode01 | Mode10 | Mode11 deriving (Eq, Enum)

data Instruction = Illegal | Instruction { useImm :: Bool, addr :: AddrMode, op :: Operation, z :: Register, x :: Register, y :: Register, imm :: Word32}

type State = (Map Register Word32, Map Word32 Word32)

readFile :: Get [Word32]
readFile = do
  skip (4 * 4) -- skip past the header
  size  <- getWord32le
  insns <- replicateM (fromIntegral size) getWord32le
  return insns

loadState :: [Word32] -> Word32 -> State -> State
loadState [] _w s = s
loadState (x:xs) w s = loadState xs (w+1) (setMem w x s)

main :: IO ()
main = do argv <- getArgs
          raw_data <- BL.readFile $ head argv
          let insns = runGet Main.readFile raw_data
          let inistate = (Data.Map.empty, Data.Map.empty)
          let loadstate = loadState insns 0x1000 inistate
          let first = toInstruction $ getMem 0x1000 loadstate
          let m = runCode first (setRegister P 0x1000 loadstate)
          putStrLn (show $ fst m)

runCode :: Instruction -> State -> State
runCode Illegal state = state
runCode insn state = do let s1 = setRegister P (getRegister P state + 1) state
                        let s2 = evalInstruction s1 insn
                        let newInsn = toInstruction $ getMem (getRegister P s2) s2
                        runCode newInsn s2

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

getRegister :: Register -> State -> Word32
getRegister A   = \_ -> 0
getRegister reg = (findWithDefault 0 reg) . fst

setRegister :: Register -> Word32 -> State -> State
setRegister A _value state  = state
setRegister reg value (regs, mem) = (insert reg value regs, mem)

getMem :: Word32 -> State -> Word32
getMem address = (findWithDefault 0 address) . snd

setMem :: Word32 -> Word32 -> State -> State
setMem address value (regs, mem) = (regs, insert address value mem)

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
    OP_BIT_OR   -> \x y -> x .|. y
    OP_BIT_AND  -> \x y -> x .&. y
    OP_ADD      -> \x y -> x + y
    OP_MUL      -> \x y -> x * y
    OP_SHIFTL   -> \x y -> x * (2 ^ y)
    OP_LT       -> \x y -> boolToReg (foo x < foo y)
    OP_EQ       -> \x y -> boolToReg (x == y)
    OP_GT       -> \x y -> boolToReg (foo x > foo y)
    OP_BIT_ANDN -> \x y -> x .&. (complement y)
    OP_BIT_XOR  -> \x y -> x `xor` y
    OP_SUB      -> \x y -> x - y
    OP_BIT_XORN -> \x y -> x `xor` (complement y)
    OP_SHIFTR   -> \x y -> x `div` (2 ^ y)
    OP_NEQ      -> \x y -> boolToReg (x /= y)

