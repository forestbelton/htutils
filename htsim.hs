import Data.Array.Unboxed
import Data.Map
import Data.Word

type Registers = UArray Register Word32
data Register  = A | B | C | D | E | F | G | H
               | I | J | K | L | M | N | O | P
  deriving (Eq, Ord, Ix, Show)

type Memory    = Map Word32 Word32
type CPU       = (Registers, Memory)

main :: IO ()
main = return ()
