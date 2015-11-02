import Utils
import Test

t 1 = test_list "test_align" [ test_equal "" 4 (align 4 3)
                             , test_equal "" 12 (align 4 11)
                             , test_equal "" 100 (align 10 91)
                             ]

main = run_all_tests t 1
