module Net.Dns.Format where
import           Data.Word (Word8)

data DnsMessage = DnsMessage [Word8]
