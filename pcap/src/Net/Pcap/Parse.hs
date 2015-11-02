module Net.Pcap.Parse ( pPcapNGFormat
                      , pPcapNGFormatFromFile) where
import Control.Monad (replicateM)
import qualified Data.ByteString as BS (readFile)
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Word (Word8, Word32, byteSwap32)
import Text.Parsec.ByteString (Parser)
import Text.ParserCombinators.Parsec (parse, many)
import Text.ParserCombinators.Parsec.Char (anyChar)
import Parse (anyByte, anyWord16, anyWord32)
import Net.Pcap.Format
import Sure
import Utils


pSectionHeaderBody = do
  { byteOrderMagic <- anyWord32
  ; majorVersion <- anyWord16
  ; minorVersion <- anyWord16
  ; sectionLength1 <- anyWord32
  ; sectionLength2 <- anyWord32
  ; options <- many anyWord32
  ; return $ SectionHeaderBody
    byteOrderMagic majorVersion minorVersion
    sectionLength1 sectionLength2 options } :: Parser Body

pInterfaceDescriptionBody = do
  { linkType <- anyWord16
  ; reserved <- anyWord16
  ; snapLen <- anyWord32
  ; options <- many anyWord32
  ; return $ InterfaceDescriptionBody
    linkType reserved snapLen options } :: Parser Body

pEnhancedPacketBody = do
  { interfaceID <- anyWord32
  ; timestampHigh <- anyWord32
  ; timestampLow <- anyWord32
  ; capturedLen <- anyWord32
  ; packetLen <- anyWord32
  ; len <- return $ align 4 . fromEnum . byteSwap32 $ capturedLen
  ; packetData <- replicateM len anyByte
  ; options <- many anyWord32
  ; return $ EnhancedPacketBody
  interfaceID timestampHigh timestampLow
  (byteSwap32 capturedLen)
  (byteSwap32 packetLen)
  packetData options } :: Parser Body

pBody bType = case byteSwap32 bType of
  0x00000001 -> pInterfaceDescriptionBody
  0x00000006 -> pEnhancedPacketBody
  0x0A0D0D0A -> pSectionHeaderBody
  otherwise -> many anyByte >>= return . Raw :: Parser Body

pBlock = do { bType <- anyWord32
            ; bLength <- anyWord32
            ; len <- return $ fromEnum (byteSwap32 bLength) - 12
            ; bData <- replicateM len anyChar
            ; tLength <- anyWord32
            ; body <- return $ sure $ parse (pBody bType) "" $ C8.pack bData
            ; return $ Block bType bLength body } :: Parser Block

pPcapNGFormat = many pBlock >>= return . PcapNGFile
pPcapNGFormatFromFile file = BS.readFile file >>= return . parse pPcapNGFormat file
