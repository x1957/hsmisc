module Db.Pg where
import           Text.Printf (printf)

data PgConfig = PgConfig { _usr :: String
                         , _psd :: String
                         , _hst :: String
                         , _por :: Int
                         , _db  :: String }
  deriving (Show, Read)

connStr (PgConfig usr psd hst por db) =
  printf "postgres://%s:%s@%s:%d/%s?sslmode=disable" usr psd hst por db :: String

defaultConfig = PgConfig "postgres" "" "localhost" 5432 "postgres"
