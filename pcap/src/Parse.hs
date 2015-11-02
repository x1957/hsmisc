module Parse (anyByte, anyWord16, anyWord32, bytes2str) where
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Data.Word (Word8, Word16, Word32, byteSwap32)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec.Char (anyChar)
import Utils

anyByte = anyChar >>= return . fromIntegral . fromEnum :: Parser Word8
pInt k = replicateM k anyByte >>= return . fromIntegral . bytes2int 256
anyWord16 = pInt 2 :: Parser Word16
anyWord32 = pInt 4 :: Parser Word32

bytes2str = C8.pack . map (chr . fromEnum :: Word8 -> Char)
