module Text.TSG.Log where

import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~), at, non, (%=), contains, (?~))
import System.IO (IO, Handle)
import Effectful (Effect, DispatchOf, Dispatch(..), Eff, IOE, (:>), runEff)
import Effectful.Dispatch.Static (StaticRep, SideEffects(..), evalStaticRep, unsafeEff_, getStaticRep)


data Logger = Logger { _level :: Int
                     , _callback :: String -> IO ()
                     , _handle :: Maybe (IO Handle)
                     }
data Log :: Effect

makeLenses ''Logger

defaultLogger = Logger { _callback = putStrLn
                       , _level = 0
                       , _handle = Nothing
                       }


type instance DispatchOf Log = Static WithSideEffects
newtype instance StaticRep Log = Log Logger

runLog :: IOE :> es => Logger -> Eff (Log : es) a -> Eff es a
runLog logger = evalStaticRep (Log logger)

log :: Log :> es => Int -> String -> Eff es ()
log ll msg = do
  Log logger <- getStaticRep
  case ll <= logger ^. level of False -> pure ()
                                True -> unsafeEff_ $ _callback logger msg
