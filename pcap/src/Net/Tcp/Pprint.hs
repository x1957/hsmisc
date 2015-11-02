module Net.Tcp.Pprint where
import qualified Data.ByteString.Char8 as C8 (pack)
import Data.Char (chr)
import Text.Printf (printf)
import Net.Tcp.Format

deriving instance Show TcpHeader

instance Show TcpPacket where
  show (TcpPacket th td) = unlines
    [ show th
    , printf "TcpBoby: %s" $ show (C8.pack $ map (chr . fromEnum) td)
    ]
