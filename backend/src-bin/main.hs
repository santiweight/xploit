import Backend
import Frontend
import Obelisk.Backend
import Common.Server.Api
import Server.Base
import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import Control.Concurrent

main :: IO ()
main = do
  migrateDB
  -- print "migration done"
  runBackend backend frontend
  -- let Right validFullEncoder = checkEncoder fullRouteEncoder
  -- run $ runFrontend validFullEncoder frontend
  pure ()