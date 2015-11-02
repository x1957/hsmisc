module Net.Ip.Parse where
import Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Data.Word (Word8)
import Text.Parsec.ByteString (Parser)
import Parse (anyByte, anyWord16, anyWord32)
import Text.ParserCombinators.Parsec (many, parse)
import Net.Ip.Format
import Parse (anyByte)
import Sure

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

decode_ipv4_packet = sure . parse pIpv4Packet "" . C8.pack . map (chr . fromEnum) :: [Word8] -> IpPacket
