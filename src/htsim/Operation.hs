module Operation where

import Control.Monad
import Data.Bits
import Data.Word

import Arch

data Operation = OP_BIT_OR    | OP_BIT_AND  | OP_ADD     | OP_MUL
               | OP_RESERVED0 | OP_SHIFTL   | OP_LT      | OP_EQ
               | OP_GT        | OP_BIT_ANDN | OP_BIT_XOR | OP_SUB
               | OP_BIT_XORN  | OP_SHIFTR   | OP_NEQ     | OP_RESERVED1
  deriving Enum

extract :: Word32 -> Int -> Int -> Word32
extract word start len = (shiftR word start) .&. (shiftL 1 len - 1)

sex :: Word32 -> Word32
sex n = n .|. (0xfffff800 * ((shiftR n 11) .&. 1))

eval :: Word32 -> CPU -> IO ()
eval word cpu = do
  let zreg = toEnum $ fromIntegral $ extract word 24 4

  x <- getReg (toEnum $ fromIntegral $ extract word 20 4) cpu
  y <- getReg (toEnum $ fromIntegral $ extract word 16 4) cpu
  z <- getReg zreg cpu

  let swap = (== 1) $ extract word 30 2
  let mode = fromIntegral $ extract word 28 2
  let f    = getOp $ toEnum $ fromIntegral $ extract word 12 4
  let imm  = sex $ extract word 0 12

  let result = if swap then f x imm + y else f x y + imm

  case mode of
    0 -> do setReg zreg result cpu
    1 -> do m <- getMem result cpu; setReg zreg m cpu
    2 -> setMem z result cpu
    3 -> setMem result z cpu

  return ()

boolToReg :: Bool -> Word32
boolToReg False = 0x00000000
boolToReg True  = 0xffffffff

-- Prepare a value for a signed comparison
prepCmp :: Word32 -> Word32
prepCmp = xor 0x80000000

getOp :: Operation -> (Word32 -> Word32 -> Word32)
getOp OP_BIT_OR   = (.|.)
getOp OP_BIT_AND  = (.&.)
getOp OP_ADD      = (+)
getOp OP_MUL      = (*)
getOp OP_SHIFTL   = (. fromIntegral) . shiftL
getOp OP_LT       = (boolToReg .) . (. prepCmp) . (<) . prepCmp
getOp OP_EQ       = (boolToReg .) . (==)
getOp OP_GT       = (boolToReg .) . (. prepCmp) . (>) . prepCmp
getOp OP_BIT_ANDN = (. complement) . (.&.)
getOp OP_BIT_XOR  = xor
getOp OP_SUB      = (-)
getOp OP_BIT_XORN = (. complement) . xor
getOp OP_SHIFTR   = (. fromIntegral) . shiftR
getOp OP_NEQ      = (boolToReg .) . (/=)

