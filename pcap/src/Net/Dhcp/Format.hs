module Net.Dhcp.Format where
import           Data.Word (Word16, Word32, Word8)

data DhcpOption = DhcpRawOption { code :: Word8
                                , len  :: Word8
                                , str  :: [Word8] }
  | Pad

data DhcpOptions = DhcpOptions { magic        :: Word32
                               , dhcp_options :: [DhcpOption] }

data DhcpMessage = DhcpMessage {
  op :: Word8, htype :: Word8, hlen :: Word8, hops :: Word8
  , xid :: Word32
  , secs :: Word16, flags :: Word16
  , ciAddr :: Word32
  , yiAddr :: Word32
  , siAddr :: Word32
  , giAddr :: Word32
  , chAddr :: (Word32, Word32, Word32, Word32)
  , sname :: [Word32] -- 64 Bytes
  , file :: [Word32] -- 128 Bytes
  , options :: DhcpOptions }

data DhcpPacket = DhcpRaw DhcpMessage
  | DhcpDiscover DhcpMessage
  | DhcpOffer DhcpMessage
  | DhcpRequest DhcpMessage
  | DhcpAck DhcpMessage
