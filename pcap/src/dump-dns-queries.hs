module Main( main
           , module Net.Pcap) where
import           Control.Monad      ((>=>))
import           Data.List          (intercalate)
import           Data.Maybe         (isJust)
import           Misc.Plain         (plain)
import           Misc.Sure          (sure)
import           Net.Dns.Format     (dnsQuestion, qEntries, qName)
import           Net.Pcap
import           System.Environment (getArgs, getProgName)

view t file = pPcapNGFormatFromFile file >>=
              return . map sure . filter isJust .
              map t . blocks . sure >>=
              mapM_ print

t_dns_queries = t_dns >=> return . plain . (intercalate ",") . (map $ intercalate "." . qName) . qEntries . dnsQuestion

dumpDnsQueries [file] = view t_dns_queries file
dumpDnsQueries _ = do
    cmd <- getProgName
    putStrLn $ "Usage:\n\t " ++ cmd ++ " <tcpdump-date>"

main = getArgs >>= dumpDnsQueries
