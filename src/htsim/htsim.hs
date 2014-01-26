import Control.Monad
import Data.IORef
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import System.Environment

import Arch
import Operation

runCode :: CPU -> IO ()
runCode cpu = do
  pc   <- getReg P cpu
  word <- getMem pc cpu -- Fetch word at program counter

  setReg P (pc + 1) cpu

  if word == 0xffffffff then
    return ()
  else
    do eval word cpu; runCode cpu

parse  :: Get [Word32]
parse = do skip (4 * 4)
           size  <- liftM fromIntegral getWord32le
           replicateM size getWord32le

boot :: [Word32] -> IO (Registers, Memory)
boot insns = do
  cpu <- initCPU
  boot' cpu 0x1000 insns    -- Load instructions into memory.
  setReg P 0x1000 cpu   -- Initialize program counter.
  runCode cpu           -- Begin program.
  readIORef cpu
 where boot' _   _    []     = return ()
       boot' cpu addr (x:xs) = do
         setMem addr x cpu
         boot' cpu (addr + 1) xs

main :: IO ()
main = do argv    <- getArgs
          bstring <- BL.readFile $ head argv
          let insns       = runGet parse bstring
          (regs, mem) <- boot insns
          print regs
