import Control.Monad.State
import qualified Data.Array.Unboxed as UA
import Data.Map
import Data.Word

data Register  = A | B | C | D | E | F | G | H
               | I | J | K | L | M | N | O | P
  deriving (Eq, Ord, UA.Ix, Show)
type Registers = UA.UArray Register Word32
type Memory    = Map Word32 Word32
type CPU       = State (Registers, Memory)

getReg :: Register -> CPU Word32
getReg A   = return 0
getReg reg = do (regs, _mem) <- get
                return $ regs UA.! reg

setReg :: Register -> Word32 -> CPU ()
setReg A   _val  = return ()
setReg reg val   = do (regs, mem) <- get
                      let regs' = regs UA.// [(reg, val)]
                      put (regs', mem)
                      return ()

main :: IO ()
main = return ()
