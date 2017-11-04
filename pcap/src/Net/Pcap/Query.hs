module Net.Pcap.Query where
import           Control.Monad    ((>=>))
import           Misc.Binary      (justDecode)
import           Net.Pcap.Filters
import           Net.Pcap.Format
import           Net.TCPIP

type Trans a b = a -> Maybe b
type T b = Trans Block b

-- transformations
t1 = Just . blockBody :: T Body

t2 = t1 >=>
     link_packet >=>
     justDecode
     :: T Frame

t2' = t2 >=>
      arp_frame >=>
      justDecode
      :: T ArpFrame

t3 = t2 >=>
     ip_frame >=>
     justDecode
     :: T IpPacket

t4 = t3 >=>
     tcp_packet >=>
     justDecode
     :: T TcpPacket

t4' = t3 >=>
      udp_packet >=>
      justDecode
      :: T UdpPacket

t5 = t4' >=>
     dhcp_packet >=>
     justDecode
     :: T DhcpPacket

t_dns = t4' >=>
        dns_packet >=>
        justDecode
        :: T DnsMessage
