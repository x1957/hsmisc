module Net.Link.Parse where
import Control.Monad (replicateM)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (many, parse)
import Net.Link.Format
import Parse (anyByte, anyWord16, anyWord32, decode_bytes_with)
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
--            ; crc <- return $ fromIntegral . bytes2int 256 $ [a,b,c,d]
            ; return $ Frame h (reverse p) 0 } :: Parser Frame

decode_link = sure . decode_bytes_with pFrame
