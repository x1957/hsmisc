module Net.Tcp.Format where
import           Data.Bits (shiftR)
import           Data.Word (Word16, Word32, Word8)

data TcpFlag = NS | CWR | ECE | URG | ACK | PUH | RST | SYN | FIN deriving (Eq, Enum, Ord)

tcp_flag_mask = shiftR 0x0100 . fromEnum :: TcpFlag -> Word16

data TcpHeader = TcpHeader { tcp_source_port :: Word16
                           , tcp_desc_port   :: Word16
                           , seq             :: Word32
                           , ack             :: Word32
                           , offset          :: Word8
                           , flags           :: [TcpFlag]
                           , window_size     :: Word16
                           , tcp_check_sum   :: Word16
                           , urg_pointer     :: Word16 }

data TcpOptionalHeader = TcpOptionalHeader [Word8]

data TcpPacket = TcpPacket { tcpHeader         :: TcpHeader
                           , tcpOptionalHeader :: Maybe TcpOptionalHeader
                           , tcp_data          :: [Word8] }