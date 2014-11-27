module Main where
import Data.List
import Database.HDBC as HDBC(quickQuery', toSql)
import Database.HDBC.Sqlite3
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp as Warp(run)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.CaseInsensitive as CI

table = ":memory:"
query = HDBC.quickQuery'

init_db conn = query conn "CREATE TABLE log(id int, msg text);" []
loginfo s = (putStr "[info] ") >> (print s)
logdb conn s = query conn "insert into log(id, msg)values(0, ?);" [toSql s]
show_lines rows = concat [show r ++ "\n" | r <- rows]

handle _ _ "/" = readFile "example.hs"
handle _ _ "/form" = return "<!doctype html><form action=\"post\"><input name=\"msg\"/></form>"
handle conn _ "/list" = query conn "SELECT * FROM log;" [] >>= \res -> return $ show_lines res
handle conn req "/post" = let str = lookup (C.pack "msg") (queryString req)
                          in  case str of
                            Just (Just s) -> query conn "INSERT INTO log(id, msg)VALUES(0, ?);" [toSql s] >>
                                             return "done"
                            _ -> return "none"
handle _ _ _ = return "hello\n"

render respond content = respond $ responseLBS status200 headers (LC.pack content) where
  headers = map f [("Content-Type", content_type)]
    where f = (\(x,y) -> (CI.mk $ C.pack x, C.pack y))
          content_type = "text/" ++ if html content then "html" else "plain" where
            html = isPrefixOf "<!doctype html>"

dbapp conn req res = (loginfo $ req) >> logdb conn "" >>
                     handle conn req (C.unpack $ rawPathInfo req) >>= (render res)

main = connectSqlite3 table >>= \conn -> init_db conn >> (run 3000 $ dbapp conn)
