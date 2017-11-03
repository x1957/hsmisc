{-# LANGUAGE ScopedTypeVariables #-}

module Net.Pcap.Query where
import           Control.Monad     ((>=>))
import           GHC.Word          (Word8)
import           Misc.Binary       (FromBytes, decode)
import           Net.Pcap.Filters
import           Net.Pcap.Format
import           Net.TCPIP
import           Text.Parsec.Error (ParseError)

type Trans a b = a -> Maybe b
type T b = Trans Block b

-- transformations
t1 = Just . blockBody :: T Body

t2 = t1 >=>
     link_packet >=>
     Just . decode_link :: T Frame

t2' = t2 >=>
      arp_frame >=>
      Just . decode_arp_frame :: T ArpFrame

t3 = t2 >=>
     ip_frame >=>
     Just . decode_ipv4_frame :: T IpPacket

t4 = t3 >=>
     tcp_packet >=>
     Just . decode_tcp_packet :: T TcpPacket

t4' = t3 >=>
      udp_packet >=>
      Just . decode_udp_packet :: T UdpPacket

t5 = t4' >=>
     dhcp_packet >=>
     Just . decode_dhcp_packet :: T DhcpPacket

t_dns = t4' >=>
        dns_packet >=>
        Just  . decode_dns_packet
      --   (justDecode :: Decoder DnsMessage)
        :: T DnsMessage

type Decoder a = [Word8] -> Maybe a

-- justDecode :: (FromBytes a) => Decoder a
-- justDecode bs =
--       case (decode bs :: Either ParseError a) of
--       Right result -> Just (result :: a)
--       _            -> Nothing
