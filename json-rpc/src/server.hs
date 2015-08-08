{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Network (serverSettings)
import Data.Time.Clock (getCurrentTime)
import Network.JsonRpc
import System.Environment (getArgs)
import Api.Time

srv :: AppConduits () () TimeRes TimeReq () () IO -> IO ()
srv (src, snk) = src $= CL.mapM respond $$ snk

respond :: IncomingMsg () TimeReq () () -> IO (Message () () TimeRes)
respond (IncomingMsg (MsgRequest (Request ver _ TimeReq i)) Nothing) = do
    t <- getCurrentTime
    return $ MsgResponse (Response ver (TimeRes t) i)

respond (IncomingError e) = return $ MsgError e
respond (IncomingMsg (MsgError e) _) = return $ MsgError $ e
respond _ = undefined

runOn port = tcpServer V2 (serverSettings port "0.0.0.0") srv
main = getArgs >>= \args -> case args of
  [p] -> runOn (read p :: Int)
  _ -> runOn 8080
