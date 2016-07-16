module Net.Udp.Format where
import           Data.Word (Word16, Word8)

data UdpHeader = UdpHeader { udp_source_port :: Word16
                           , udp_desc_port   :: Word16
                           , udp_length      :: Word16
                           , udp_checksum    :: Word16 }

data UdpPacket = UdpPacket { udp_header :: UdpHeader
                           , udp_data   :: [Word8] }
