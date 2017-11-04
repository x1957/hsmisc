module Net.Pcap.Parse where

import qualified Data.ByteString               as BS (readFile)
import           Misc.Parse                    (anyWord16, anyWord32, pByteN)
import           Net.Pcap.Format
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many, parse)


pGlobalHeader = do { magic <- anyWord32
                   ; versionMajor <- anyWord16
                   ; versionMinor <- anyWord16
                   ; thisZone <- anyWord32
                   ; sigfigs <- anyWord32
                   ; snaplen <- anyWord32
                   ; network <- anyWord32
                   ; return $ GlobalHeader
                     magic
                     versionMajor
                     versionMinor
                     thisZone
                     sigfigs
                     snaplen
                     network
                   } :: Parser GlobalHeader

pPacketHeader = do { tsSec   <- anyWord32
                   ; tsUsec  <- anyWord32
                   ; inclLen <- anyWord32
                   ; origLen <- anyWord32
                   ; return $ PacketHeader
                     tsSec
                     tsUsec
                     inclLen
                     origLen
                   } :: Parser PacketHeader

pBlock = do { packetHeader <- pPacketHeader
            ; packetData <- pByteN (inclLen packetHeader)
            ; return $ Block packetHeader packetData
            } :: Parser Block

pPcapFile = do { globalHeader <- pGlobalHeader
               ; blocks <- many pBlock
               ; return $ PcapFile globalHeader blocks
               } :: Parser PcapFile

pPcapFromFile file = fmap (parse pPcapFile file) (BS.readFile file)
