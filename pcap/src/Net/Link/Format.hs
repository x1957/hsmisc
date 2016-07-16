module Net.Link.Format where
import           Data.Bits
import           Data.Word        (Word16, Word32, Word8)
import           Data.Word.Compat (byteSwap32)

data MacAddress = MacAddress Word8 Word8 Word8 Word8 Word8 Word8

data MacHeader = MacHeader { desc      :: MacAddress
                           , src       :: MacAddress
                           , etherType :: Word16 }

data Frame = Frame { macHeader :: MacHeader
                   , payload   :: [Word8]
                   , crc       :: Word32 }
