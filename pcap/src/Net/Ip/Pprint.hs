module Net.Ip.Pprint where
import           Misc.Utils    (show_chunks)
import           Net.Bits      (qword2words)
import           Net.Ip.Format
import           Net.Tcp       (decode_tcp_packet)
import           Text.Printf   (printf)

instance Show IPv4Addr where
  show (IPv4Addr ip) = printf "%d.%d.%d.%d" a b c d where [a,b,c,d] = qword2words ip

proto_name 0x01 = "ICMP"
proto_name 0x02 = "IGMP"
proto_name 0x06 = "TCP"
proto_name 0x11 = "UDP"
proto_name _    = "?"

instance Show IpHeader where
  show ihv4 = unlines
    [ printf "version: %d" (version ihv4)
    , printf "ihl %d" (ihl ihv4)
    , printf "tot_len %d" (tot_len ihv4)
    , printf "identification %d" (identification ihv4)
    , printf "flags %d" (ip_flags ihv4)
    , printf "offset %d" (offset ihv4)
    , printf "ttl %d" (ttl ihv4)
    , let pn = protocol ihv4
      in  printf "protocol %d (%s)" pn (proto_name pn)
    , printf "check_sum 0x%04x" (check_sum ihv4)
    , printf "%s -> %s"
      (show $ src_addr ihv4)
      (show $ dst_addr ihv4)
    ]

show_ip_packet p d = case p of
  0x06 -> show $ decode_tcp_packet d
  _    -> "non-tcp ip packet"

instance Show IpPacket where
  show (IpPacket h d) = unlines
    [ show h
    , show_chunks 8 d
    ]
