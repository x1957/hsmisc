{-# LANGUAGE ScopedTypeVariables #-}

module Query where

import           Control.Monad      ((>=>))
import           Data.Word          (Word8)
import           Misc.Binary        (justDecode)
import           Net.PcapNg.Filters (arp_frame, dhcp_packet, dns_packet,
                                     icmp_packet, ip_frame, tcp_packet,
                                     udp_packet)
import           Net.TCPIP          (ArpFrame, DhcpPacket, DnsMessage, Frame,
                                     IcmpPacket, IpPacket, TcpPacket, UdpPacket)


type LinkPacket = [Word8]
type Query a = LinkPacket -> Maybe a

class Queryable a where
  query :: Query a

instance Queryable Frame where
  query = justDecode

instance Queryable ArpFrame where
  query = (query :: Query Frame) >=> arp_frame >=> justDecode

instance Queryable IpPacket where
  query = (query :: Query Frame) >=> ip_frame >=> justDecode

instance Queryable IcmpPacket where
  query = (query :: Query IpPacket) >=> icmp_packet >=> justDecode

instance Queryable TcpPacket where
  query = (query :: Query IpPacket) >=> tcp_packet >=> justDecode

instance Queryable UdpPacket where
  query = (query :: Query IpPacket) >=> udp_packet >=> justDecode

instance Queryable DhcpPacket where
  query = (query :: Query UdpPacket) >=> dhcp_packet >=> justDecode

instance Queryable DnsMessage where
  query = (query :: Query UdpPacket) >=> dns_packet >=> justDecode
