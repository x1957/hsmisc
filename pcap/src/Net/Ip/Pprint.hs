module Net.Ip.Pprint where
import Data.Bits (shiftR)
import Data.Word (Word8, Word32)
import Net.Ip.Format
import Net.Tcp
import Text.Printf (printf)
import Utils

oclets :: Word32 -> [Word8]
oclets x = map (fromIntegral . fromEnum) [x `shiftR` 24, x `shiftR` 16, x `shiftR` 8, x]

show_ip :: Word32 -> String
show_ip ip = printf "%d.%d.%d.%d" a b c d where [a,b,c,d] = oclets ip

instance Show IpHeader where
  show ihv4 = unlines
    [ printf "version: %d" (version ihv4)
    , printf "ihl %d" (ihl ihv4)
    , printf "tot_len %d" (tot_len ihv4)
    , printf "identification %d" (identification ihv4)
    , printf "flags %d" (flags ihv4)
    , printf "offset %d" (offset ihv4)
    , printf "ttl %d" (ttl ihv4)
    , printf "protocol %d" (protocol ihv4)
    , printf "check_sum 0x%04x" (check_sum ihv4)
    , printf "%s -> %s"
      (show_ip $ source_addr ihv4)
      (show_ip $ desc_addr ihv4)
    ]

show_ip_packet p d = case p of
  0x06 -> show $ decode_tcp_packet d
  otherwise -> "non-tcp ip packet"

instance Show IpPacket where
  show (IpPacket h d) = unlines
    [ show h
    , show_chunks 4 d
    ]
