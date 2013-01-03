
module Aptx.Image.Bmp(createBmp) where
import Data.Int
import Data.Char
import Data.Binary
import qualified Data.ByteString.Lazy.Char8 as L8

createBmp :: Int -> Int -> [[Int]] -> L8.ByteString
createBmp width height mat = createBmpWithSkip width height skip mat where
    skip = head $ filter (\n -> mod n 4  == 0) [width * 3 .. ]
    createBmpWithSkip w h s mat = L8.concat [
          bmpMagic
        , bmpHead
        , bmpInfo
        , bmpData
        ] where
        bmpMagic = L8.pack "BM"
        bmpHead = L8.concat [
              fileSize
            , creater1
            , creater2
            , offset
            ] where
                fileSize = L8.reverse $ encode (fromIntegral (s * h + 54) :: Int32)
                creater1 = L8.reverse $ encode (0 :: Int16)
                creater2 = L8.reverse $ encode (0 :: Int16)
                offset   = L8.reverse $ encode (54 :: Int32)
        bmpInfo = L8.concat [
              header_size
            , width
            , height
            , nplanes
            , bitspp
            , compress_type
            , image_size
            , hres
            , vres
            , ncolors
            , nimpcolors
            ] where
                header_size = L8.reverse $ encode (40 :: Int32)
                width       = L8.reverse $ encode (fromIntegral w :: Int32)
                height      = L8.reverse $ encode (fromIntegral h :: Int32)
                nplanes     = L8.reverse $ encode (1 :: Int16)
                bitspp      = L8.reverse $ encode (24 :: Int16)
                compress_type = L8.reverse $ encode (0 :: Int32)
                image_size  = L8.reverse $ encode (fromIntegral(s * h) :: Int32)
                hres        = L8.reverse $ encode (0 :: Int32)
                vres        = L8.reverse $ encode (0 :: Int32)
                ncolors     = L8.reverse $ encode (0 :: Int32)
                nimpcolors  = L8.reverse $ encode (0 :: Int32)
        bmpData = makeBmpData w h s mat pad where
            pad = replicate (s - w * 3) $ L8.pack "0"
            makeBmpData w h s mat pad = L8.concat (map makeRow mat) where
                makeRow row = L8.concat ((map createPixel row) ++ pad) where
                    createPixel x = L8.concat $ map encode ([b, g, r] :: [Int8]) where
                        r = fromIntegral (div x 65536) :: Int8
                        g = fromIntegral (mod (div x 256) 256) :: Int8
                        b = fromIntegral (mod x 256) :: Int8

