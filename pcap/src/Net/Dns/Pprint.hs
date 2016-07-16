{-# LANGUAGE StandaloneDeriving #-}
module Net.Dns.Pprint where
import           Net.Dns.Format

deriving instance Show DnsMessage
