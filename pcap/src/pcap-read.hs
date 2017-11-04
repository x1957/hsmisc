{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           App.Usage          (appF)
import           Control.Monad      ((>=>))
import           Data.Maybe         (mapMaybe)
import           Misc.Sure          (sure)
import qualified Net.Pcap           as Pcap ()
import           Net.PcapNg         as PcapNg (PcapNGFile, blockBody, blocks,
                                               link_packet,
                                               pPcapNGFormatFromFile, pcapInfo)
import           Net.TCPIP          (ArpFrame, DhcpPacket, DnsMessage, Frame,
                                     IcmpPacket, IpPacket, TcpPacket, UdpPacket,
                                     icmp_header, ipHeader, macHeader, tcp_data)
import           Query              (Query, query)
import           System.Environment (getArgs)
import           Text.Parsec.Error  (ParseError)

actions :: [([String], FilePath -> IO ())]
actions = [ (["info"], info (pcapInfo . sure))
          , ([], view Just)
          ] ++
          queries

queries =
          [ (["eth"], view (query :: Query Frame))
          , (["eth", "header"], view ((query :: Query Frame) >=>
                                      Just . macHeader))
          , (["arp"], view (query :: Query ArpFrame))
          , (["ip"], view (query :: Query IpPacket))
          , (["ip", "header"], view ((query:: Query IpPacket) >=>
                                     Just . ipHeader))
          , (["icmp"], view (query :: Query IcmpPacket))
          , (["icmp", "header"], view ((query :: Query IcmpPacket) >=>
                                       Just . icmp_header))
          , (["tcp"], view (query :: Query TcpPacket))
          , (["tcp", "data"], view ((query :: Query TcpPacket) >=> Just . tcp_data))
          , (["udp"], view (query :: Query UdpPacket))
          , (["dhcp"], view (query :: Query DhcpPacket))
          , (["dns"], view (query :: Query DnsMessage))
          ]

type ParseResult a = Either ParseError a

info :: Show a => (ParseResult PcapNGFile -> a) -> FilePath -> IO ()
info t file = fmap t (pPcapNGFormatFromFile file) >>= print

view :: forall a . Show a => Query a -> FilePath -> IO ()
view q file = fmap (runQuery q) (pPcapNGFormatFromFile file :: IO (ParseResult PcapNGFile)) >>=
              (mapM_ print :: [a] -> IO ())

runQuery :: Query a -> ParseResult PcapNGFile -> [a]
runQuery q = mapMaybe (Just . blockBody >=> link_packet >=> q) . blocks . sure

main = getArgs >>= appF actions
