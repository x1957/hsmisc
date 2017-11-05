{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad      ((>=>))
import           Data.List          (intercalate)
import           Data.Maybe         (mapMaybe)
import           Misc.Parse         (ParseResult)
import           Misc.Plain         (plain)
import           Misc.Sure          (sure)
import           Net.Dns.Format     (DnsMessage, dnsQuestion, qEntries, qName)
import           Net.PcapNg         (PcapNGFile, pPcapNGFormatFromFile)
import           Query              (Query, linkPackets, query)
import           System.Environment (getArgs, getProgName)

view :: forall a . Show a => Query a -> FilePath -> IO ()
view q file = fmap (runQuery q) (pPcapNGFormatFromFile file :: IO (ParseResult PcapNGFile)) >>=
              (mapM_ print :: [a] -> IO ())

runQuery :: Query a -> ParseResult PcapNGFile -> [a]
runQuery q = mapMaybe q . linkPackets . sure

queriedDomains = plain . intercalate "," . map (intercalate "." . qName) . qEntries . dnsQuestion

dumpDnsQueries [file] = view ((query :: Query DnsMessage) >=> return . queriedDomains) file
dumpDnsQueries _ = do
    cmd <- getProgName
    putStrLn $ "Usage:\n\t " ++ cmd ++ " <tcpdump-data>"

main = getArgs >>= dumpDnsQueries
