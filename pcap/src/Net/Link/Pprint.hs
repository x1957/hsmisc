module Net.Link.Pprint where
import Net.Link.Format
import Net.Ip
import Text.Printf (printf)
import Utils

instance Show MacAddress where
  show (MacAddress o1 o2 o3 o4 o5 o6) = printf "%02x:%02x:%02x:%02x:%02x:%02x"
                                        o1 o2 o3 o4 o5 o6

instance Show MacHeader where
  show (MacHeader m1 m2 et) = printf "%s -> %s 0x%04x" (show m1)  (show m2) et

show_payload et p = case et of
  0x0800 -> show $ decode_ipv4_packet p
  otherwise -> printf "etherType: 0x%04x" et

instance Show Frame where
  show (Frame h p c) = unlines
    [ printf "mac header: %s" (show h)
    , printf "crc 0x%08x" c
    , "Payload:"
    , printf "%s" (show_chunks 4 p)
    ]
