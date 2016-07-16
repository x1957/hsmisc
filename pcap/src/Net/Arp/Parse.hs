module Net.Arp.Parse where
import           Misc.Parse                    (anyByte, anyWord16, anyWord32,
                                                decode_bytes_with)
import           Misc.Sure
import           Net.Arp.Format
import           Net.Link.Parse                (pMacAddress)
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (parse)

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
