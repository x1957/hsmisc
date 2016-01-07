module Net.Ip.FormatTest where
import Net.Ip.Format
import Test

test_version_and_ihl f h l  = test_list "test_version_and_ihl"
                              [ test_equal "" h (version ihv4)
                              , test_equal "" l (ihl ihv4)]
  where ihv4 = Ipv4Header f 1 2 3 4 5 6 7 8 9

test_dscp_and_ecn f h l = test_list "test_dscp_and_ecn"
                          [ test_equal "" h (dscp ihv4)
                          , test_equal "" l (ecn ihv4)]
  where ihv4 = Ipv4Header 0 f 2 3 4 5 6 7 8 9

test_flags_and_offset f h l  = test_list "test_flags_and_offset"
                               [ test_equal "" h (flags ihv4)
                               , test_equal "" l (offset ihv4)]
  where ihv4 = Ipv4Header 0 1 2 3 f 5 6 7 8 9

t 1 = test_version_and_ihl 1 0 1
t 2 = test_version_and_ihl 16 1 0
t 3 = test_dscp_and_ecn 1 0 1
t 4 = test_dscp_and_ecn 4 1 0
t 5 = test_flags_and_offset 1 0 1
t 6 = test_flags_and_offset 0x1000 0 0x1000
t 7 = test_flags_and_offset 0x2000 1 0

main = run_all_tests t 7
