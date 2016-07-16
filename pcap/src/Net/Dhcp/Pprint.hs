{-# LANGUAGE StandaloneDeriving #-}
module Net.Dhcp.Pprint where
import           Net.Dhcp.Format

deriving instance Show DhcpOption
deriving instance Show DhcpOptions
deriving instance Show DhcpMessage
deriving instance Show DhcpPacket
