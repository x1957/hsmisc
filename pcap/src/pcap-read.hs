module Main where

import           App.Usage          (appF)
import           Control.Monad      ((>=>))
import           Data.Maybe         (catMaybes)
import           Misc.Binary        (justDecode)
import           Misc.Sure          (sure)
import qualified Net.Pcap           as Pcap ()
import           Net.PcapNg         as PcapNg
import           Net.TCPIP          (IcmpPacket, icmp_header, ipHeader,
                                     macHeader, tcp_data)
import           System.Environment (getArgs)
import           Text.Parsec.Error  (ParseError)

actions = [ (["eth"], view t2)
          , (["eth", "header"], view (t2 >=>
                                      Just . macHeader))
          , (["arp"], view t2')
          , (["ip"], view t3)
          , (["ip", "header"], view (t3 >=>
                                     Just . ipHeader))
          , (["icmp"], view (t3 >=>
                             icmp_packet >=>
                             justDecode :: T IcmpPacket))
          , (["icmp", "header"], view (t3 >=>
                                       icmp_packet >=>
                                       justDecode >=>
                                       Just . icmp_header))
          , (["tcp"], view t4)
          , (["tcp", "data"], view (t4 >=> Just . tcp_data))
          , (["udp"], view t4')
          , (["dhcp"], view t5)
          , (["dhcp", "data"], view (t4' >=> dhcp_packet))
          , (["dns"], view t_dns)
          , (["info"], info (pcapInfo . sure))
          , ([], view t1)]

info :: Show a =>
     (Either ParseError PcapNGFile -> a)
     -> FilePath -> IO ()
info t file = fmap t (pPcapNGFormatFromFile file) >>= print

view :: Show a => (Block -> Maybe a) -> FilePath -> IO ()
view t file = pPcapNGFormatFromFile file >>=
              return . catMaybes .
              map t . blocks . sure >>=
              mapM_ print

main = getArgs >>= appF actions
