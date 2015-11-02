module Net.Link.Format where
import Data.Bits
import Data.Word (Word8, Word16, Word32, byteSwap32)


data MacAddress = MacAddress Word8 Word8 Word8 Word8 Word8 Word8

data MacHeader = MacHeader { desc :: MacAddress
                           , src :: MacAddress
                           , etherType :: Word16 }

data Frame = Frame { macHeader :: MacHeader
                   , payload :: [Word8]
                   , crc :: Word32 }
