module Net.Ip.Format where
import Data.Bits ((.&.), shiftR)
import Data.Word (Word8, Word16, Word32, byteSwap32)

data IpHeader = Ipv4Header { _version_and_ihl :: Word8  -- 4, 4
                           , _dscp_and_ecn :: Word8  -- 6, 2
                           , tot_len :: Word16
                           , identification :: Word16
                           , _flags_and_offset :: Word16  -- 3, 13
                           , ttl :: Word8
                           , protocol :: Word8
                           , check_sum :: Word16
                           , source_addr :: Word32
                           , desc_addr :: Word32 }

data IpPacket = IpPacket { ipHeader :: IpHeader
                         , ipData :: [Word8] }

version = (flip shiftR 4) . _version_and_ihl
ihl = (.&. 0x0f) . _version_and_ihl

dscp = (flip shiftR 2) . _dscp_and_ecn
ecn = (.&. 0x03) . _dscp_and_ecn

flags = (flip shiftR 13) . _flags_and_offset
offset = (.&. 0x1fff) . _flags_and_offset
