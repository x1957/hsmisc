module Main( main
--           , module Net.TCPIP
           , module Net.Pcap) where
import           App.Usage
import           Control.Monad      ((>=>))
import           Data.Maybe         (isJust)
import           Misc.Binary        (decode)
import           Misc.Sure
import           Net.Pcap
import           Net.TCPIP
import           System.Environment (getArgs)
import           Text.Printf        (printf)

actions = [ (["eth"], view t2)
          , (["eth", "header"], view (t2 >=> Just . macHeader))
          , (["arp"], view t2')
          , (["ip"], view t3)
          , (["ip", "header"], view (t3 >=> Just . ipHeader))
          , (["icmp"], view (t3 >=> icmp_packet >=>
                             Just . sure . decode >=>
                             Just . (id :: IcmpPacket -> IcmpPacket)))
          , (["icmp", "header"], view (t3 >=> icmp_packet >=>
                                       Just . sure . decode >=>
                                       Just . icmp_header))
          , (["tcp"], view t4)
          , (["tcp", "data"], view (t4 >=> Just . tcp_data))
          , (["udp"], view t4')
          , (["dhcp"], view t5)
          , (["info"], info (pcapInfo . sure))
          , ([], view t1)]

info t file = pPcapNGFormatFromFile file >>= return . t >>= print

view t file = pPcapNGFormatFromFile file >>=
              return . map sure . filter isJust .
              map t . blocks . sure >>=
              mapM_ print

main = getArgs >>= appF actions