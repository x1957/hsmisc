module Net.Tcp.Format where
import Data.Word (Word8, Word16, Word32)

data TcpHeader = TcpHeader { source_port :: Word16
                           , desc_port :: Word16
                           , seq :: Word32
                           , ack :: Word32
                           , _offset_and_flags :: Word16
                           , window_size :: Word16
                           , tcp_check_sum :: Word16
                           , urg_pointer :: Word16 }

data TcpOptionalHeader = TcpOptionalHeader [Word8]

data TcpPacket = TcpPacket { tcpHeader :: TcpHeader
                           , tData :: [Word8] }
