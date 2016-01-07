import Control.Monad ((>=>))
import Data.Maybe (isJust)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)
import Net.Pcap
import Net.Link
import Net.Arp
import Net.Ip
import Net.Tcp
import Net.Udp
import Sure

actions = [ (["eth"], view t2)
          , (["eth", "header"], view (t2 >=> Just . macHeader))
          , (["arp"], view t2')
          , (["ip"], view t3)
          , (["ip", "header"], view (t3 >=> Just . ipHeader))
          , (["tcp"], view t4)
          , (["udp"], view t4') ]

main = getArgs >>= \args -> case args of
  [] -> usage
  [file] -> view t1 file
  ["info", file] -> info (pcapInfo . sure) file
  otherwise -> let (file : args') = reverse args
               in  case lookup (reverse args') actions of
    Just a -> a file
    otherwise -> usage

usage = getProgName >>= mapM_ putStrLn . helps where
  helps cmd = "Usage:" :
    [ "\t" ++ (unwords $ cmd : cmds ++ ["<file>"])
    | cmds <- [] : ["info"] : map fst actions ]

info t file = pPcapNGFormatFromFile file >>= return . t >>= print
view t file = pPcapNGFormatFromFile file >>=
              return . map sure . filter isJust .
              map t . blocks . sure >>=
              mapM_ print

t1 = Just . blockBody
t2 = t1 >=> link_packet >=> Just . decode_link
t2' = t2 >=> arp_frame >=> Just . decode_arp_frame
t3 = t2 >=> ip_frame >=> Just . decode_ipv4_frame
t4 = t3 >=> tcp_packet >=> Just . decode_tcp_packet
t4' = t3 >=> udp_packet >=> Just . decode_udp_packet
