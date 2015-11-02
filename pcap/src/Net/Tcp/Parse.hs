module Net.Tcp.Parse where
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Data.Word (Word8)
import Text.Parsec.ByteString (Parser)
import Parse (anyByte, anyWord16, anyWord32)
import Text.ParserCombinators.Parsec (many, parse)
import Net.Tcp.Format
import Parse (anyByte, anyWord16, anyWord32)
import Sure


pTcpHeader = do { sp <- anyWord16
                ; dp <- anyWord16
                ; seq <- anyWord32
                ; ack <- anyWord32
                ; _ofs <- anyWord16
                ; ws <- anyWord16
                ; cs <- anyWord16
                ; up <- anyWord16
                ; return $ TcpHeader dp sp seq ack _ofs ws cs up } :: Parser TcpHeader

pTcpPacket = do { th <- pTcpHeader
                ; td <- many anyByte
                ; return $ TcpPacket th td } :: Parser TcpPacket

decode_tcp_packet = sure . parse pTcpPacket "" . C8.pack . map (chr . fromEnum) :: [Word8] -> TcpPacket
