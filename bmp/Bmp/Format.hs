module Bmp.Format( create_bmp
                 , pack_bmp_file
                 , pixel
                 , writeBmpFile) where
import Data.Binary (encode)
import Data.Word (Word8, Word16, Word32)
import Data.ByteString.Lazy as BS(writeFile)
import qualified Data.ByteString.Lazy.Char8 as C8 (concat, pack, reverse)


data Header = Header { file_size :: Word32
                     , creater1 :: Word16
                     , creater2 :: Word16
                     , offset :: Word32 }
              deriving (Show)

data Info = Info { header_size :: Word32
                 , width :: Word32
                 , height :: Word32
                 , nplanes :: Word16
                 , bitspp :: Word16
                 , compress_type :: Word32
                 , image_size :: Word32
                 , hres :: Word32
                 , vres :: Word32
                 , ncolors :: Word32
                 , nimpcolors :: Word32 }
          deriving (Show)

data Pixel = Pixel { r :: Word8
                   , g :: Word8
                   , b :: Word8 }
           deriving (Show)

data Bmp = Bmp { magic :: [Char]
               , header :: Header
               , info :: Info
               , bitmap :: [[Pixel]] }

pixel x = Pixel r g b where
  r = fromIntegral (div x 65536)
  g = fromIntegral (mod (div x 256) 256)
  b = fromIntegral (mod x 256)

create_bmp width height mat = Bmp "BM" header info (map (map pixel) mat) where
  skip = head . filter ((== 0) . flip mod 4) $ [width * 3 .. ]
  header = Header (skip * height + 54) 0 0 54
  info = Info 40 width height 1 24 0 (skip * height) 0 0 0 0

pack_header (Header x y z w) = C8.concat . map C8.reverse $ [encode x, encode y, encode z, encode w]
pack_info (Info x1 x2 x3 x4 x4' x5 x6 x7 x8 x9 x10) = C8.concat . map C8.reverse . concat $
                                                      [ map encode [x1, x2, x3]
                                                      , map encode [x4, x4']
                                                      , map encode [x5, x6, x7, x8, x9, x10] ]
pack_bitmap padding = C8.concat . map encode . concatMap ((++ padding) . concatMap p)
  where p x = [b, g, r] where Pixel r g b = x

pack_bmp_file (Bmp magic header info bitmap) = C8.concat [ C8.pack magic
                                                         , pack_header header
                                                         , pack_info info
                                                         , pack_bitmap padding bitmap ] where
  w = width info
  s = head . filter ((== 0) . flip mod 4) $ [w * 3 .. ]
  padding = replicate (fromIntegral s - fromIntegral w * 3) 0

writeBmpFile = (. pack_bmp_file) . BS.writeFile
