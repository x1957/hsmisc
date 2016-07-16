module Net.Pcap.Query where
import           Control.Monad    ((>=>))
import           Net.Pcap.Filters
import           Net.Pcap.Format
import           Net.TCPIP

-- transformations
t1 = Just . blockBody
t2 = t1 >=> link_packet >=> Just . decode_link
t2' = t2 >=> arp_frame >=> Just . decode_arp_frame
t3 = t2 >=> ip_frame >=> Just . decode_ipv4_frame
t4 = t3 >=> tcp_packet >=> Just . decode_tcp_packet
t4' = t3 >=> udp_packet >=> Just . decode_udp_packet
t5 = t4' >=> dhcp_data >=> Just . decode_dhcp_packet
  where dhcp_data = Just . udp_data
