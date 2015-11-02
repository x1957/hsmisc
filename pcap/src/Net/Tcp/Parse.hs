module Net.Tcp.Parse where
import Text.Parsec.ByteString (Parser)
import Parse (anyByte, anyWord16, anyWord32)
import Text.ParserCombinators.Parsec (many, parse)
import Net.Tcp.Format
import Parse (anyByte, anyWord16, anyWord32, bytes2str)
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

decode_tcp_packet = sure . parse pTcpPacket "" .bytes2str
