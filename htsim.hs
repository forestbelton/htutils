import Control.Monad.State
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Word
import System.Environment

import Arch
import Operation

runCode :: CPU ()
runCode = do
  pc   <- getReg P
  word <- getMem pc
  setReg P (pc + 1)
--  trace ("word: " ++ (showHex word "")) $ return ()
  case word of
    0xffffffff ->
      return ()
    _ ->
      do eval word
         (regs, _mem) <- get
--         trace ("regs: " ++ (show regs)) $ return ()
         runCode

parse  :: Get [Word32]
parse = do skip (4 * 4)
           size  <- liftM fromIntegral getWord32le
           replicateM size getWord32le

boot :: [Word32] -> CPU (Registers, Memory)
boot insns = do
  boot' 0x1000 insns    -- Load instructions into memory.
  setReg P 0x1000       -- Initialize program counter.
  runCode               -- Begin program.
  get                   -- Return final state.
 where boot' addr []     = return ()
       boot' addr (x:xs) = do
         setMem addr x
         boot' (addr + 1) xs

main :: IO ()
main = do argv    <- getArgs
          bstring <- BL.readFile $ head argv
          let insns       = runGet parse bstring
          let (regs, mem) = evalState (boot insns) initCPU
          putStrLn $ show regs
          return ()
