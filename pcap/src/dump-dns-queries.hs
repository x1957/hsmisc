module Main where

import           Control.Monad      ((>=>))
import           Data.List          (intercalate)
import           Data.Maybe         (catMaybes)
import           Misc.Plain         (plain)
import           Misc.Sure          (sure)
import           Net.Dns.Format     (dnsQuestion, qEntries, qName)
import           Net.PcapNg         (blocks, pPcapNGFormatFromFile, t_dns)
import           System.Environment (getArgs, getProgName)

view t file = pPcapNGFormatFromFile file >>=
              return . catMaybes .
              map t . blocks . sure >>=
              mapM_ print

t_dns_queries = t_dns >=>
                return . plain . (intercalate ",") . (map $ intercalate "." . qName) . qEntries . dnsQuestion

dumpDnsQueries [file] = view t_dns_queries file
dumpDnsQueries _ = do
    cmd <- getProgName
    putStrLn $ "Usage:\n\t " ++ cmd ++ " <tcpdump-data>"

main = getArgs >>= dumpDnsQueries
