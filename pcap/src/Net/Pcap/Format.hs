-- https://wiki.wireshark.org/Development/LibpcapFileFormat

module Net.Pcap.Format where

import           Data.Word (Word16, Word32, Word8)

data GlobalHeader = GlobalHeader { magic        :: Word32
                                 , versionMajor :: Word16
                                 , versionMinor :: Word16
                                 , thisZone     :: Word32 -- Int32
                                 , sigfigs      :: Word32
                                 , snaplen      :: Word32
                                 , network      :: Word32
                                 }

data PacketHeader = PacketHeader { tsSec   :: Word32
                                 , tsUsec  :: Word32
                                 , inclLen :: Word32
                                 , origLen :: Word32
                                 }

data Block = Block { packetHeader :: PacketHeader
                   , packetData   :: [Word8]
                   }

data PcapFile = PcapFile { globalHeader :: GlobalHeader
                         , blocks       :: [Block] }
