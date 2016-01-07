module Net.Udp.Parse where
import Net.Udp.Format
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (many, parse)
import Parse (anyByte, anyWord16, anyWord32, decode_bytes_with)
import Sure

pUdpHeader = do { sp <- anyWord16
                ; dp <- anyWord16
                ; len <- anyWord16
                ; sum <- anyWord16
                ; return $ UdpHeader sp dp len sum } :: Parser UdpHeader

pUdpPacket = do { uh <- pUdpHeader
                ; ud <- many anyByte
                ; return $ UdpPacket uh ud } :: Parser UdpPacket

decode_udp_packet = sure . decode_bytes_with pUdpPacket
