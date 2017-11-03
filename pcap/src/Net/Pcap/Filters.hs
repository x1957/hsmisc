module Net.Pcap.Filters where
import           GHC.Word        (Word16, Word8)
import           Net.Ip
import           Net.Link
import           Net.Pcap.Format
import           Net.Tcp
import           Net.Udp

if' x y z = if x then y else z
just p q = if' p (Just q) Nothing
select p q = just (p q) q

link_packet db = case db of
  EnhancedPacketBody _ _ _ cl _ pd _ -> Just $ take (fromEnum cl) pd
  _                                  -> Nothing

ether_type et (Frame mh p _) = just (et == (etherType mh)) p

arp_frame, ip_frame :: Frame -> Maybe [Word8]
arp_frame = ether_type 0x0806
ip_frame = ether_type 0x0800

ip_type :: Word8 -> IpPacket -> Maybe [Word8]
ip_type it (IpPacket h d) = just (it == (protocol h)) d

icmp_packet, tcp_packet, udp_packet :: IpPacket -> Maybe [Word8]
icmp_packet = ip_type 1
tcp_packet = ip_type 6
udp_packet = ip_type 17

dns_packet :: UdpPacket -> Maybe [Word8]
dns_packet (UdpPacket hdr udp_data) =
      if udp_source_port hdr == 53 || udp_desc_port hdr == 53
      then Just udp_data
      else Nothing

from_tcp_port :: Word16 -> TcpPacket -> Maybe Word16
from_tcp_port n = select (n==) . tcp_source_port . tcpHeader
to_tcp_port n = select (n==) . tcp_desc_port . tcpHeader
from_udp_port n = select (n==) . udp_source_port . udp_header
to_udp_port n = select (n==) . udp_desc_port . udp_header

-- dhcp_discovery =
