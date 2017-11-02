module Net.Ip.Parse where
import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, anyWord16, anyWord32,
                                                decode_bytes_with)
import           Misc.Sure
import           Net.Ip.Format
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many)

pIpv4Header = do { vi <- anyByte
                 ; de <- anyByte
                 ; tl <- anyWord16
                 ; i <- anyWord16
                 ; fo <- anyWord16
                 ; ttl <- anyByte
                 ; p <- anyByte
                 ; cs <- anyWord16
                 ; src <- anyWord32
                 ; desc <- anyWord32
                 ; return $ Ipv4Header vi de tl i fo ttl p cs src desc } :: Parser IpHeader

pIpv4Packet = do { ihv4 <- pIpv4Header
                 ; ipData <- many anyByte
                 ; return $ IpPacket ihv4 ipData } :: Parser IpPacket

decode_ipv4_frame = sure . decode_bytes_with pIpv4Packet

instance FromBytes IpHeader where
  decode = decode_bytes_with pIpv4Header

instance FromBytes IpPacket where
  decode = decode_bytes_with pIpv4Packet
