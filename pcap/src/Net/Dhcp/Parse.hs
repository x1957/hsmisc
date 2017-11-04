module Net.Dhcp.Parse where
import           Control.Monad                 (replicateM)
import           Data.Word                     (Word8)
import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, anyWord16, anyWord32,
                                                decode_bytes_with, pByteN,
                                                pStringN)
import           Misc.Sure                     (sure)
import           Net.Dhcp.Format
import           Net.Ip.Parse                  (pIPv4Addr)
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many)

pDHCPMessageType :: Parser DHCPMessageType
pDHCPMessageType = do
  byte <- anyByte
  return (toEnum . fromEnum $ byte :: DHCPMessageType)

pDhcpOptionWithCode :: Word8 -> Parser DhcpOption
pDhcpOptionWithCode code = do
  len <- anyByte
  case code of
    1  -> fmap SubnetMask pIPv4Addr
    2  -> fmap TimeOffset anyWord32
    3  -> fmap RouterOption (many pIPv4Addr)
    4  -> fmap TimeServerOption (many pIPv4Addr)
    5  -> fmap NameServerOption (many pIPv4Addr)
    6  -> fmap DomainNameServerOption (many pIPv4Addr)
    7  -> fmap LogServerOption (many pIPv4Addr)
    8  -> fmap CookieServerOption (many pIPv4Addr)
    9  -> fmap LPRServerOption (many pIPv4Addr)
    10 -> fmap ImpressServerOption (many pIPv4Addr)
    11 -> fmap ResourceLocationServerOption (many pIPv4Addr)
    12 -> fmap HostNameOption (pStringN len)

    50 -> fmap RequestedIPAddress pIPv4Addr
    51 -> fmap IPAddressLeaseTime anyWord32
    52 -> fmap OptionOverload anyWord16
    53 -> fmap DHCPMessageTypeOption pDHCPMessageType
    54 -> fmap ServerIdentifier pIPv4Addr
    55 -> fmap ParameterRequestList (pByteN len)
    56 -> fmap Message (pStringN len)
    57 -> fmap MaximumDHCPMessageSize anyWord16
    61 -> fmap ClientIdentifier (pByteN len)
    _  -> fmap (DhcpRawOption code len) (pByteN len)


pDhcpOption = do { code <- anyByte
                 ; case code of
                     0   -> return Pad
                     255 -> return End
                     _   -> pDhcpOptionWithCode code } :: Parser DhcpOption

pDhcpOptions = do { magic <- anyWord32
                  ; options <- many pDhcpOption
                  ; return $ DhcpOptions magic options } :: Parser DhcpOptions

pDhcpMessage = do { [op, htype, hlen, hops] <- replicateM 4 anyByte
                  ; xid <- anyWord32
                  ; [secs, flags] <- replicateM 2 anyWord16
                  ; [ciAddr, yiAddr, siAddr, giAddr] <- replicateM 4 pIPv4Addr
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
