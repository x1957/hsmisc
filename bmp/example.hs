import Data.List.Split (chunksOf)
import System.Random (getStdGen, randomRs)
import Bmp

gen_blocks w h rs = take h $ chunksOf w rs

gen_colors n = randomRs (0 :: Int, n)

w = 18
h = 12

main :: IO ()
main = getStdGen >>=
       return . gen_colors 0xffffff >>=
       return . gen_blocks w h >>=
       return . BM w h >>=
       return . bold 36 >>=
       return . toBmp >>=
       writeBmpFile "blocks.bmp"
