module Net.Arp.Parse where
import           Misc.Binary            (FromBytes (..))
import           Misc.Parse             (anyByte, anyWord16, anyWord32,
                                         decode_bytes_with)
import           Misc.Sure              (sure)
import           Net.Arp.Format
import           Net.Link.Parse         (pMacAddress)
import           Text.Parsec.ByteString (Parser)

pArpFrame = do { ht <- anyWord16
               ; pt <- anyWord16
               ; hl <- anyByte
               ; pl <- anyByte
               ; op <- anyWord16
               ; sha <- pMacAddress
               ; spa <- anyWord32
               ; tha <- pMacAddress
               ; tpa <- anyWord32
               ; return $ ArpFrame ht pt hl pl op sha spa tha tpa } :: Parser ArpFrame

decode_arp_frame = sure . decode_bytes_with pArpFrame

instance FromBytes ArpFrame where
  decode = decode_bytes_with pArpFrame
