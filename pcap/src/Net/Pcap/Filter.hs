module Net.Pcap.Filter where
import Net.Pcap.Format
import Net.Link
import Net.Ip
import qualified Net.Tcp as T
import qualified Net.Udp as U

if' x y z = if x then y else z
just p q = if' p (Just q) Nothing
select p q = just (p q) q

link_packet db = case db of
  EnhancedPacketBody _ _ _ cl _ pd _ -> Just $ take (fromEnum cl) pd
  otherwise -> Nothing

ether_type et (Frame mh p _) = just (et == (etherType mh)) p

arp_frame = ether_type 0x0806
ip_frame = ether_type 0x0800

ip_type it (IpPacket h d) = just (it == (protocol h)) d

tcp_packet = ip_type 6
udp_packet = ip_type 17

from_tcp_port n = select (n==) . T.source_port . T.tcpHeader
to_tcp_port n = select (n==) . T.desc_port . T.tcpHeader
from_udp_port n = select (n==) . U.source_port . U.udpHeader
to_udp_port n = select (n==) . U.desc_port . U.udpHeader
