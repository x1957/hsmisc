module Bmp.Bitmap ( bold
                  , bbold
                  , toBmp
                  , Bitmap(BM)) where
import Bmp.Format

data Bitmap a = BM { width :: Int
                   , height :: Int
                   , array :: [[a]] }

bold1 d = concat . map (replicate d)

bold2 w h = bold1 w . map (bold1 h)

bold d (BM w h a) = BM (w * d) (h * d) (bold2 d d a)


bbold1 b c d = foldl1 f . map (replicate d) where f x y = x ++ (replicate b c) ++ y

bbold' b c d = foldl1 f . map (replicate d) . map (bbold1 b c d)
  where f x y = x ++ replicate b (map (const c) $ head x) ++ y

bbold b c d (BM w h a) = BM (w * (b + d) - b) (h * (b + d) - b) (bbold' b c d a)

toBmp (BM w h a) = create_bmp (fromIntegral w) (fromIntegral h) a
