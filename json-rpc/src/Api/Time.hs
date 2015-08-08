{-# LANGUAGE OverloadedStrings #-}
module Api.Time(TimeReq(..), TimeRes(..)) where
import Data.Aeson.Types (ToJSON, toJSON, emptyArray, withText)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, parseTime)
import Network.JsonRpc (FromRequest, ToRequest, FromResponse, paramsParser, parseResult, requestMethod)
import System.Locale (defaultTimeLocale)

data TimeReq = TimeReq
data TimeRes = TimeRes UTCTime

instance FromRequest TimeReq where
    paramsParser "time" = Just $ const $ return TimeReq
    paramsParser _ = Nothing

instance ToRequest TimeReq where
    requestMethod TimeReq = "time"

instance ToJSON TimeReq where
    toJSON TimeReq = emptyArray

instance FromResponse TimeRes where
    parseResult "time" = withText "time" $ \t -> case f t of
        Nothing -> fail "Could not parse time"
        Just t' -> return $ TimeRes t'
      where
        f t = parseTime defaultTimeLocale "%c" (T.unpack t)

instance ToJSON TimeRes where
    toJSON (TimeRes t) = toJSON $ formatTime defaultTimeLocale "%c" t
