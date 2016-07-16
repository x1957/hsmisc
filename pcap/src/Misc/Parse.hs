module Misc.Parse (anyByte, anyWord16, anyWord32, decode_bytes_with) where
import           Control.Monad                      (replicateM)
import qualified Data.ByteString.Char8              as C8 (pack)
import           Data.Char                          (chr)
import           Data.Word                          (Word16, Word32, Word8)
import           Misc.Utils
import           Text.Parsec.ByteString             (Parser)
import           Text.ParserCombinators.Parsec      (parse)
import           Text.ParserCombinators.Parsec.Char (anyChar)

anyByte = anyChar >>= return . fromIntegral . fromEnum :: Parser Word8
pInt k = replicateM k anyByte >>= return . fromIntegral . bytes2int 256
anyWord16 = pInt 2 :: Parser Word16
anyWord32 = pInt 4 :: Parser Word32

bytes2str = C8.pack . map (chr . fromEnum :: Word8 -> Char)

decode_bytes_with p = parse p "" . bytes2str
