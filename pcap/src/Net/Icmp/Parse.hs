module Net.Icmp.Parse() where
import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, anyWord16, anyWord32,
                                                decode_bytes_with)
import           Net.Icmp.Format
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many)

pIcmpHeader = do { t <- anyByte
                 ; c <- anyByte
                 ; sum <- anyWord16
                 ; rest <- anyWord32
                 ; return $ IcmpHeader t c sum rest } :: Parser IcmpHeader

pIcmpPacket = do { h <- pIcmpHeader
                 ; d <- many anyByte
                 ; return $ IcmpPacket h d } :: Parser IcmpPacket

instance FromBytes IcmpHeader where
  decode = decode_bytes_with pIcmpHeader

instance FromBytes IcmpPacket where
  decode = decode_bytes_with pIcmpPacket
