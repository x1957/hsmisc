import Control.Monad ((>=>))
import Data.Maybe (isJust)
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)
import Net.Pcap
import Net.Link
import Net.Ip
import Net.Tcp
import Sure

main = getArgs >>= \args -> case args of
  [file] -> view t1 file
  ["eth", file] -> view t2 file
  ["ip", file] -> view t3 file
  ["tcp", file] -> view t4 file
  otherwise -> getProgName >>= mapM_ putStrLn . help

view t file = pPcapNGFormatFromFile file >>=
              return . map sure . filter isJust .
              map t . blocks . sure >>=
              mapM_ print

t1 = Just . blockBody
t2 = t1 >=> link_packet >=> Just . decode_link
t3 = t2 >=> ip_frame >=> Just . decode_ipv4_packet
t4 = t3 >=> tcp_packet >=> Just . decode_tcp_packet

help cmd = [ "Usage:"
           , "\t " ++ cmd ++ " <file>"
           , "\t " ++ cmd ++ " eth <file>"
           , "\t " ++ cmd ++ " ip <file>"
           , "\t " ++ cmd ++ " tcp <file>"]
