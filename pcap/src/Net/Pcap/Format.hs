module Net.Pcap.Format where
import Data.Word (Word8, Word16, Word32)
import Data.Word.Compat (byteSwap32)

data Option = Option { optionCode :: Word16
                     , optionLength :: Word16
                     , optionValue :: [Word8] }

data Body = Raw [Word8]
          | SectionHeaderBody { byteOrderMagic :: Word32
                              , majorVersion :: Word16
                              , minorVersion :: Word16
                              , sectionLength1 :: Word32
                              , sectionLength2 :: Word32
                              , raw_options :: [Word32] }
          | InterfaceDescriptionBody { linkType :: Word16
                                     , reserved :: Word16
                                     , snapLen :: Word32
                                     , raw_options :: [Word32] }
          | EnhancedPacketBody { interfaceID :: Word32
                               , timestampHigh :: Word32
                               , timestampLow :: Word32
                               , capturedLen :: Word32
                               , packetLen :: Word32
                               , packetData :: [Word8]
                               , options :: [Option] }
          | SimplePacketBlock { simple_packetLen :: Word32
                              , simple_packetData :: [Word32] }
          | PacketBlock [Word32] -- obsolete
          | NameResolutionBlock {}

data Block = Block { blockType :: Word32
                   , blockLength :: Word32
                   , blockBody :: Body }

data PcapNGFile = PcapNGFile { blocks :: [Block] }

data PcapNGFileInfo = PcapNGFileInfo { num_of_blocks :: Int } deriving (Show)

pcapInfo (PcapNGFile blocks) = PcapNGFileInfo (length blocks)
