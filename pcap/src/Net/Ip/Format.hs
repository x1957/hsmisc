module Net.Ip.Format where
import           Data.Bits (shiftR, (.&.))
import           Data.Word (Word16, Word32, Word8)

data IpFlags = DF | MF deriving (Eq, Ord, Enum)

newtype IPv4Addr = IPv4Addr Word32

data IpHeader = Ipv4Header { _version_and_ihl  :: Word8  -- 4, 4
                           , _dscp_and_ecn     :: Word8  -- 6, 2
                           , tot_len           :: Word16
                           , identification    :: Word16
                           , _flags_and_offset :: Word16  -- 3, 13
                           , ttl               :: Word8
                           , protocol          :: Word8
                           , check_sum         :: Word16
                           , src_addr          :: IPv4Addr
                           , dst_addr          :: IPv4Addr }

data IpPacket = IpPacket { ipHeader :: IpHeader
                         , ipData   :: [Word8] }

version = (flip shiftR 4) . _version_and_ihl
ihl = (.&. 0x0f) . _version_and_ihl

dscp = (flip shiftR 2) . _dscp_and_ecn
ecn = (.&. 0x03) . _dscp_and_ecn

ip_flags = (flip shiftR 13) . _flags_and_offset
offset = (.&. 0x1fff) . _flags_and_offset
