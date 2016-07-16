{-# LANGUAGE StandaloneDeriving #-}
module Net.Udp.Pprint where
import           Net.Udp.Format

deriving instance Show UdpHeader
deriving instance Show UdpPacket
