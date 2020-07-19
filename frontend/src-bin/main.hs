import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import Common.DB.Base

main :: IO ()
main = do
  migrateDB
  -- runBackend
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  run $ runFrontend validFullEncoder frontend
