import Text.ParserCombinators.Parsec (parse)
import Data.ByteString.Char8 as C8 (pack)
import Parse
import Sure
import Test

t 1 = test_list "test_anyByte" [ test_equal "" 0 (p "\0\0")
                               , test_equal "" 0 (p "\0\1")
                               , test_equal "" 1 (p "\1\0")
                               , test_equal "" 1 (p "\1")
                               ] where
  p = sure . parse anyByte "" . pack

t 2 = test_list "test_anyWord16" [ test_equal "" 0 (p "\0\0")
                                 , test_equal "" 1 (p "\0\1")
                                 , test_equal "" 256 (p "\1\0")
                                 , test_equal "" 257 (p "\1\1")
                                 ] where
  p = sure . parse anyWord16 "" . pack

t 3 = test_list "test_anyWord32" [ test_equal "" 0 (p "\0\0\0\0")
                                 , test_equal "" 1 (p "\0\0\0\1")
                                 , test_equal "" 256 (p "\0\0\1\0")
                                 , test_equal "" 65536 (p "\0\1\0\0")
                                 , test_equal "" 16777216 (p "\1\0\0\0")
                                 , test_equal "" 16909060 (p "\1\2\3\4")
                                 ] where
  p = sure . parse anyWord32 "" . pack

t 4 = test_list "test_decode_bytes_with" [ test_equal "" b (p [b]) | b <- [0..255] ] where
  p = sure . decode_bytes_with anyByte

main = run_all_tests t 4
