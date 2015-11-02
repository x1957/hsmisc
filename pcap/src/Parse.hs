module Parse (anyByte, anyWord16, anyWord32) where
import Control.Monad (replicateM)
import Data.Word (Word8, Word16, Word32, byteSwap32)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec.Char (anyChar)
import Utils

anyByte = anyChar >>= return . fromIntegral . fromEnum :: Parser Word8
pInt k = replicateM k anyByte >>= return . fromIntegral . bytes2int 256
anyWord16 = pInt 2 :: Parser Word16
anyWord32 = pInt 4 :: Parser Word32
