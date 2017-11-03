-- https://tools.ietf.org/html/rfc1541
module Net.Dhcp.Format where
import           Data.Word (Word16, Word32, Word8)

-- https://tools.ietf.org/html/rfc2132
-- DHCP Options and BOOTP Vendor Extensions
data DhcpOption = DhcpRawOption { code :: Word8
                                , len  :: Word8
                                , str  :: [Word8] }
                | Pad -- https://tools.ietf.org/html/rfc2132#section-3.1
                | End -- https://tools.ietf.org/html/rfc2132#section-3.2

data DhcpOptions = DhcpOptions { magic        :: Word32 -- 0x63825363
                               , dhcp_options :: [DhcpOption] }

data DhcpMessage = DhcpMessage { op      :: Word8
                               , htype   :: Word8
                               , hlen    :: Word8
                               , hops    :: Word8
                                 --
                               , xid     :: Word32
                                 --
                               , secs    :: Word16
                               , flags   :: Word16
                                 --
                               , ciAddr  :: Word32 -- client IP address
                               , yiAddr  :: Word32 -- your IP address
                               , siAddr  :: Word32 -- server IP address
                               , giAddr  :: Word32 -- gateway IP address switched by relay
                               , chAddr  :: (Word32, Word32, Word32, Word32)

                                 -- 192 octets of 0s. BOOTP legacy
                               , sname   :: [Word32] -- 64 Bytes
                               , file    :: [Word32] -- 128 Bytes

                               , options :: DhcpOptions }

data DhcpPacket = DhcpRaw DhcpMessage
                | DhcpDiscover DhcpMessage
                | DhcpOffer DhcpMessage
                | DhcpRequest DhcpMessage
                | DhcpAck DhcpMessage
