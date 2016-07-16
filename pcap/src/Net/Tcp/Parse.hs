module Net.Tcp.Parse where
import           Control.Monad                 (replicateM)
import           Data.Bits                     (shiftR, (.&.))
import           Data.Word                     (Word16, Word8)
import           Misc.Binary                   (FromBytes (..))
import           Misc.Parse                    (anyByte, anyWord16, anyWord32,
                                                decode_bytes_with)
import           Misc.Sure
import           Net.Tcp.Format
import           Text.Parsec.ByteString        (Parser)
import           Text.ParserCombinators.Parsec (many, parse)

_tcp_offset = fromIntegral . fromEnum . (`shiftR` 12) :: Word16 -> Word8
_tcp_flags _ofs = [ f | f <- [NS .. ], ((tcp_flag_mask f) .&. _ofs) /= 0 ]

pTcpHeader = do { sp <- anyWord16
                ; dp <- anyWord16
                ; seq <- anyWord32
                ; ack <- anyWord32
                ; _ofs <- anyWord16
                ; (offset, flags) <- return $ (_tcp_offset _ofs, _tcp_flags _ofs)
                ; ws <- anyWord16
                ; cs <- anyWord16
                ; up <- anyWord16
                ; return $ TcpHeader dp sp seq ack
                  offset flags ws cs up } :: Parser TcpHeader

pTcpOptionalHeader offset = replicateM offset anyByte >>=
  return . TcpOptionalHeader :: Parser TcpOptionalHeader

pTcpPacket = do { th <- pTcpHeader
                ; oh <- let len = offset th
                        in if len > 5
                           then (pTcpOptionalHeader $ (fromEnum len - 5) * 4) >>=
                                return . Just
                           else return Nothing
                ; td <- many anyByte
                ; return $ TcpPacket th oh td } :: Parser TcpPacket

decode_tcp_packet = sure . decode_bytes_with pTcpPacket

instance FromBytes TcpHeader where
  decode = decode_bytes_with pTcpHeader

instance FromBytes TcpPacket where
  decode = decode_bytes_with pTcpPacket
