{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad      ((>=>))
import           Data.List          (intercalate)
import           Data.Maybe         (mapMaybe)
import           Misc.Plain         (plain)
import           Misc.Sure          (sure)
import           Net.Dns.Format     (DnsMessage, dnsQuestion, qEntries, qName)
import           Net.PcapNg         (PcapNGFile, blockBody, blocks, link_packet,
                                     pPcapNGFormatFromFile)
import           Query              (Query, query)
import           System.Environment (getArgs, getProgName)
import           Text.Parsec.Error  (ParseError)

type ParseResult a = Either ParseError a

view :: forall a . Show a => Query a -> FilePath -> IO ()
view q file = fmap (runQuery q) (pPcapNGFormatFromFile file :: IO (ParseResult PcapNGFile)) >>=
              (mapM_ print :: [a] -> IO ())

runQuery :: Query a -> ParseResult PcapNGFile -> [a]
runQuery q = mapMaybe (Just . blockBody >=> link_packet >=> q) . blocks . sure

t_dns_queries = (query :: Query DnsMessage) >=>
                return . plain . (intercalate ",") . map (intercalate "." . qName) . qEntries . dnsQuestion

dumpDnsQueries [file] = view t_dns_queries file
dumpDnsQueries _ = do
    cmd <- getProgName
    putStrLn $ "Usage:\n\t " ++ cmd ++ " <tcpdump-data>"

main = getArgs >>= dumpDnsQueries
