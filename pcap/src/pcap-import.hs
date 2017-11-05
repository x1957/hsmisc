import           App.Usage                (appF)
import           Codec.Binary.Base64      (encode)
import           Data.Maybe               (mapMaybe)
import           Database.HDBC            as HDBC
import           Database.HDBC.PostgreSQL
import           Db.Pg
import           Misc.Sure                (sure)
import           Net.PcapNg
import           Net.TCPIP
import qualified Query                    as Q
import           System.Environment       (getArgs, getEnv)

query = HDBC.quickQuery'

main = getArgs >>= action

action ["config"] = print defaultConfig
action ["drop"] = with_conn $ \conn -> query conn sql []
  where sql = "drop table if exists ip_packet;"
action ["init"] = with_conn $ \conn -> query conn sql []
  where sql = ("create table ip_packet " ++
                "(id serial unique, tot_len int, " ++
                "identification int, protocol int2, " ++
                "check_sum int, src_addr cidr, dst_addr cidr, " ++
                "payload bytea, payload_base64 text);")

action a = appF actions a where actions = [ ([], memoryF)]

with_conn f = getDbConfig >>= return . connStr >>= connectPostgreSQL >>= \conn -> f conn >> commit conn

memoryF file = with_conn $ \conn -> memory conn file

memory conn file =
  pPcapNGFormatFromFile file >>=
  return . mapMaybe (Q.query :: Q.Query IpPacket) . Q.linkPackets . sure >>=
  mapM_ (save_ip_packet conn)

save_ip_packet conn ipp =
  query conn ip_sql [ toSql . fromEnum . ipPacketTotLen $ iph
                    , toSql . fromEnum . protocol $ iph
                    , toSql . show . ipSrcAddr $ iph
                    , toSql . show . ipDstAddr $ iph
                    , toSql . encode . ipData $ ipp ]
  where
    ip_sql = "insert into ip_packet (tot_len, protocol, src_addr, dst_addr, payload_base64) values (?, ?, ?, ?, ?);"
    iph = ipHeader ipp

getDbConfig = getEnv "HOME" >>= \home -> readFile (home ++ "/.pgconfig") >>= \f -> return (read f :: PgConfig)
