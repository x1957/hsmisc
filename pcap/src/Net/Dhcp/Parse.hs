module Net.Dhcp.Parse where
import           Control.Monad                 (replicateM)
import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, anyWord16, anyWord32,
                                                decode_bytes_with)
import           Misc.Sure
import           Net.Dhcp.Format
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many, parse)

pDhcpOption' code = do { len <- anyByte
                       ; str <- replicateM (fromEnum len) anyByte
                       ; return $ DhcpRawOption
                         code len str } :: Parser DhcpOption

pDhcpOption = do { code <- anyByte
                 ; if code == 0 then return Pad
                   else pDhcpOption' code } :: Parser DhcpOption

pDhcpOptions = do { magic <- anyWord32
                  ; options <- many pDhcpOption
                  ; return $ DhcpOptions magic options } :: Parser DhcpOptions

pDhcpMessage = do { [op, htype, hlen, hops] <- replicateM 4 anyByte
                  ; xid <- anyWord32
                  ; [secs, flags] <- replicateM 2 anyWord16
                  ; [ciAddr, yiAddr, siAddr, giAddr] <- replicateM 4 anyWord32
                  ; [ch1, ch2, ch3, ch4] <- replicateM 4 anyWord32
                  ; sname <- replicateM 16 anyWord32
                  ; file <- replicateM 32 anyWord32
                  ; options <- pDhcpOptions
                  ; return $ DhcpMessage op htype hlen hops
                    xid secs flags
                    ciAddr yiAddr siAddr giAddr
                    (ch1, ch2, ch3, ch4)
                    sname file options } :: Parser DhcpMessage

pDhcpPacket = do { m <- pDhcpMessage
                 ; return $ DhcpRaw m } :: Parser DhcpPacket

decode_dhcp_packet = sure . decode_bytes_with pDhcpPacket

instance FromBytes DhcpPacket where
  decode = decode_bytes_with pDhcpPacket
