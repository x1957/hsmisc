module Net.Icmp.Format where
import           Data.Word (Word16, Word32, Word8)

data IcmpHeader = IcmpHeader { icmp_type        :: Word8
                             , icmp_code        :: Word8
                             , icmp_checksum    :: Word16
                             , icmp_rest_header :: Word32 }

data IcmpPacket = IcmpPacket { icmp_header :: IcmpHeader
                             , icmp_data   :: [Word8] }
