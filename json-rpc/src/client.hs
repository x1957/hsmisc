{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Time.Clock (UTCTime)
import Network.JsonRpc
import System.Environment (getArgs)
import Api.Time

cli :: AppConduits TimeReq () () () () TimeRes IO -> IO UTCTime
cli (src, snk) = do
    CL.sourceList [MsgRequest $ buildRequest V2 TimeReq] $$ snk
    ts <- src $$ CL.consume
    case ts of
        [] -> error "No response received"
        [IncomingError (ErrorObj _ m _ _ _)] -> error $ "Unknown: " ++ m
        [IncomingMsg (MsgError (ErrorObj _ m _ _ _)) _] -> error m
        [IncomingMsg (MsgResponse (Response _ (TimeRes t) _)) _] -> return t
        _ -> undefined


talkTo host port = tcpClient V2 True (clientSettings port host) cli >>= print
localhost = "127.0.0.1"
main = getArgs >>= \args -> case args of
  [p] -> talkTo localhost (read p :: Int)
  _ -> talkTo localhost 8080
