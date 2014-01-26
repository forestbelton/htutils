module Arch where

import Data.Array.Unboxed
import Data.IntMap hiding ((!))
import Data.IORef
import Data.Word
import Unsafe.Coerce

data Register = A | B | C | D | E | F | G | H
              | I | J | K | L | M | N | O | P
  deriving (Eq, Ord, Ix, Show, Enum)

type Registers = UArray Register Word32
type Memory    = IntMap Word32
type CPU       = IORef (Registers, Memory)

getReg :: Register -> CPU -> IO Word32
getReg A   _   = return 0
getReg reg cpu = do
  (regs, _) <- readIORef cpu
  return $ regs ! reg

setReg :: Register -> Word32 -> CPU -> IO ()
setReg A   _   _   = return ()
setReg reg val cpu = modifyIORef' cpu $ \(regs, mem) -> (regs // [(reg, val)], mem)

getMem :: Word32 -> CPU -> IO Word32
getMem addr cpu = do
  (_, mem) <- readIORef cpu
  return $ findWithDefault 0 (unsafeCoerce addr) mem

setMem :: Word32 -> Word32 -> CPU -> IO ()
setMem addr val cpu = modifyIORef' cpu $ \(regs, mem) -> (regs, insert (unsafeCoerce addr) val mem)

initCPU :: IO CPU
initCPU = newIORef (regs, mem)
  where regs = array (A, P) []
        mem  = empty
