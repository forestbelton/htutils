import Control.Monad.State
import qualified Data.Array.Unboxed as UA
import Data.Bits
import Data.Map
import Data.Word

data Register  = A | B | C | D | E | F | G | H
               | I | J | K | L | M | N | O | P
  deriving (Eq, Ord, UA.Ix, Show, Enum)
type Registers = UA.UArray Register Word32
type Memory    = Map Word32 Word32
type CPU       = State (Registers, Memory)

getReg :: Register -> CPU Word32
getReg A   = return 0
getReg reg = do
  (regs, _mem) <- get
  return $ regs UA.! reg

setReg :: Register -> Word32 -> CPU ()
setReg A   _val  = return ()
setReg reg val   = do
  (regs, mem) <- get
  let regs' = regs UA.// [(reg, val)]
  put (regs', mem)

getMem :: Word32 -> CPU Word32
getMem addr = do
  (_regs, mem) <- get
  return $ findWithDefault 0 addr mem

setMem :: Word32 -> Word32 -> CPU ()
setMem addr val = do
  (regs, mem) <- get
  let mem' = insert addr val mem
  put (regs, mem')

extract :: Word32 -> Int -> Int -> Word32
extract word start len = (shiftR word start) .&. (shiftL 1 len - 1)

sex :: Word32 -> Word32
sex n = n .|. (0xfffff800 * ((shiftR n 11) .&. 1))

eval :: Word32 -> CPU ()
eval word = case mode of
  0 -> do res <- result; setReg z res
  1 -> do res <- result; m <- getMem res; setReg z m
  2 -> do res <- result; dst <- getReg z; setMem dst res
  _ -> do res <- result; src <- getReg z; setMem res src
  where swap   = (== 1) $ extract word 30 2
        mode   = fromIntegral $ extract word 28 2
        z      = toEnum $ fromIntegral $ extract word 24 4
        x      = getReg $ toEnum $ fromIntegral $ extract word 20 4
        y      = getReg $ toEnum $ fromIntegral $ extract word 16 4
        f      = liftM2 $ getOp $ fromIntegral $ extract word 12 4
        imm    = return $ sex $ extract word 0 12
        addM   = liftM2 (+)
        result = if swap then f x imm `addM` y else f x y `addM` imm

runCode :: CPU ()
runCode = do
  pc <- liftM (+1) $ getReg P
  setReg P pc
  word <- getMem pc
  case word of
    0xffffffff ->
      return ()
    _ ->
      do eval word
         runCode

getOp :: Int -> (Word32 -> Word32 -> Word32)
getOp 0 = \x y -> x .|. y
getOp 1 = \x y -> x .&. y
getOp 2 = \x y -> x + y
getOp 3 = \x y -> x * y
getOp 5 = \x y -> x * (2 ^ y)
--getOp 6 = \x y -> boolToReg (foo x < foo y)
--getOp 7 = \x y -> boolToReg (x == y)
--getOp 8 = \x y -> boolToReg (foo x > foo y)
getOp 9 =  \x y -> x .&. (complement y)
--    OP_BIT_XOR -> \x y -> x `xor` y
--    OP_SUB -> \x y -> x - y
--    OP_BIT_XORN -> \x y -> x `xor` (complement y)
--    OP_SHIFTR -> \x y -> x `div` (2 ^ y)
--    OP_NEQ -> \x y -> boolToReg (x /= y)

main :: IO ()
main = return ()
