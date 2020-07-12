import Backend
import Frontend
import Obelisk.Backend
import API.Server

main :: IO ()
main = do
  forkIO $ runServer 8081
  runBackend backend frontend
