module Net.Link.Parse where
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Data.Word (Word8)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (many, parse)
import Parse (anyByte, anyWord16, anyWord32)
import Net.Link.Format
import Sure
import Utils

pMacAddress = do { [o1, o2, o3, o4, o5, o6] <- replicateM 6 anyByte
                 ; return $ MacAddress o1 o2 o3 o4 o5 o6 } :: Parser MacAddress

pMacHeader = do { m1 <- pMacAddress
                ; m2 <- pMacAddress
                ; et <- anyWord16
                ; return $ MacHeader m1 m2 et } :: Parser MacHeader

pFrame = do { h <- pMacHeader
            ; t <- many anyByte
            ; p <- return $ reverse t
--            ; d:c:b:a:p <- return $ reverse t
--            ; crc <- return $ fromIntegral . bytes2int 4 $ [a,b,c,d]
            ; return $ Frame h (reverse p) 0 } :: Parser Frame

decode_link = sure . parse pFrame "" . C8.pack . map (chr . fromEnum) :: [Word8] -> Frame
