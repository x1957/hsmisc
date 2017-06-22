import           Control.Concurrent.MVar   (MVar, newMVar, putMVar, takeMVar)
import qualified Data.ByteString.Char8     as C8 (unpack)
import qualified Data.ByteString.Char8     as C8 (pack)
import           Data.ByteString.Lazy.UTF8 as U (fromString)
import qualified Data.CaseInsensitive      as CI (mk)
import           Network.HTTP.Types        (status200)
import           Network.Wai               (Request (..), Response, rawPathInfo)
import           Network.Wai               (responseLBS)
import           Network.Wai.Handler.Warp  as Warp (run)
import           Prelude                   hiding (init)
import           System.Environment        (getArgs)
import           Text.Printf               (printf)

logInfof :: Show a => (a -> String) -> a -> IO a
logInfof fmt x = putStrLn ("[INFO] " ++ fmt x) >> return x

data TextPlain = TextPlain String

instance Mime TextPlain where
    contentType _ = "text/plain; charset=UTF-8"
    content (TextPlain str) = str

class Mime a where
      content :: a -> String
      contentType :: a -> String

send respond mime = respond $ responseLBS status200 headers (U.fromString $ content mime)
    where headers = map f [("Content-Type", contentType mime)]
          f (x,y) = (CI.mk $ C8.pack x, C8.pack y)


runWebApp app = fmap parsePort getArgs >>=
       logInfof (printf "http://localhost:%d/") >>=
       flip run app
  where parsePort [x] = read x :: Int
        parsePort _   = 3000

data State = State  { n :: Int }
data Msg = Inc

instance Show State where
    show (State n) = show n

init :: State
init = State 0

update :: Msg -> State -> State
update Inc (State n) = State { n = n + 1 }

updateM :: Msg -> MVar State -> IO State
updateM msg state = do
    oldState <- takeMVar state
    let newState = update msg oldState
    putMVar state newState
    return newState

handle :: MVar State -> t -> String -> IO TextPlain
handle state _ path = do
    state <- updateM Inc state
    return $ TextPlain $ path ++ " " ++show state

app :: MVar State -> Request -> (Response -> IO b) -> IO b
app state req res = handle state req (C8.unpack $ rawPathInfo req) >>= send res

main = do
    state <- newMVar $ init
    runWebApp $ app state
