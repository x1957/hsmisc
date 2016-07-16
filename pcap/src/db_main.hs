import           App.Usage
import qualified Data.ByteString          as B
import           Data.Maybe               (isJust)
import           Database.HDBC            as HDBC
import           Database.HDBC.PostgreSQL
import           Db.Pg
import           Misc.Sure
import           Net.Pcap
import           Net.TCPIP
import           System.Environment       (getArgs, getEnv)

query = HDBC.quickQuery'

main = getArgs >>= action

action ["config"] = print defaultConfig
action a = appF actions a
  where actions = [ ([], memoryF)]

dropSql = "drop table if exists ip_packet;"
initSql = "create table ip_packet (id serial unique, tot_len int, identification int, protocol int2, check_sum int, src_addr cidr, dst_addr cidr, payload bytea);"

initDb conn = do
  query conn dropSql []
  query conn initSql []

memoryF file = do
  cfg <- getDbConfig
  cs <- return $ connStr cfg
  conn <- connectPostgreSQL cs
  initDb conn
  memory conn file
  commit conn

memory conn file =
  pPcapNGFormatFromFile file >>=
  return . sure >>=
  return . map sure . filter isJust . map t3 . blocks >>=
  mapM_ (save_ip_packet conn)

save_ip_packet conn ipp =
  query conn ip_sql [ toSql . fromEnum . tot_len $ iph
                    , toSql . fromEnum . protocol $ iph
                    , toSql . show_ip . src_addr $ iph
                    , toSql . show_ip . dst_addr $ iph ]
  where
    ip_sql = "insert into ip_packet (tot_len, protocol, src_addr, dst_addr) values (?, ?, ?, ?);"
    iph = ipHeader ipp

getDbConfig = do
  home <- getEnv "HOME"
  f <- readFile (home ++ "/.pgconfig")
  return (read f :: PgConfig)
