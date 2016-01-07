module Net.Udp.Format where
import Data.Word (Word8, Word16)

data UdpHeader = UdpHeader { source_port :: Word16
                           , desc_port :: Word16
                           , length :: Word16
                           , checksum :: Word16 }

data UdpPacket = UdpPacket { udpHeader :: UdpHeader
                           , udpData :: [Word8] }
