import Text.ParserCombinators.Parsec (parse)
import Data.ByteString.Char8 as C8 (pack)
import Parse
import Sure
import Test

t 1 = test_list "test_anyWord16" [ test_equal "" 0 (p "\0\0")
                                 , test_equal "" 1 (p "\0\1")
                                 , test_equal "" 256 (p "\1\0")
                                 ] where
  p = sure . parse anyWord16 "" . pack

t 2 = test_list "test_anyWord32" [ test_equal "" 0 (p "\0\0\0\0")
                                 , test_equal "" 1 (p "\0\0\0\1")
                                 , test_equal "" 16909060 (p "\1\2\3\4")
                                 ] where
  p = sure . parse anyWord32 "" . pack

main = run_all_tests t 2
