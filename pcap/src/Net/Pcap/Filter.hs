module Net.Pcap.Filter where
import Net.Pcap.Format
import Net.Link
import Net.Ip

link_packet db = case db of
  EnhancedPacketBody _ _ _ cl _ pd _ -> Just $ take (fromEnum cl) pd
  otherwise -> Nothing

arp_frame = ether_type 0x0806
ip_frame = ether_type 0x0800

ether_type et (Frame mh p _) = case etherType mh of
  et -> Just p
  otherwise -> Nothing

tcp_packet (IpPacket h d) = case protocol h of
  6 -> Just d
  otherwise -> Nothing
