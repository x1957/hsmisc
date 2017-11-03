module Net.Link.Pprint where
import           Net.Link.Format
--import Net.Arp.Parse
import           Misc.Utils
import           Net.Ip
import           Text.Printf     (printf)

instance Show MacAddress where
  show (MacAddress o1 o2 o3 o4 o5 o6) = printf "%02x:%02x:%02x:%02x:%02x:%02x"
                                        o1 o2 o3 o4 o5 o6

name 0x0800 = "IP"
name 0x0806 = "ARP"
name _      = "?"

instance Show MacHeader where
  show (MacHeader m1 m2 et) = printf "%s <- %s 0x%04x (%s)" (show m1)  (show m2) et (name et)

show_payload et p = case et of
  0x0800 -> show $ decode_ipv4_frame p
--  0x0806 -> show $ decode_arp_frame p
  _      -> printf "etherType: 0x%04x" et

instance Show Frame where
  show (Frame h p c) = unlines
    [ printf "mac header: %s" (show h)
    , printf "crc 0x%08x" c
    , "Payload:"
    , printf "%s" (show_chunks 4 p)
    ]
