{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import

import Control.Concurrent (forkIO)
import Control.Monad.Logger (runLoggingT)
import Data.Default (def)
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, conduitManagerSettings)
import Network.Wai.Logger (clockDateCacher)
import Network.Wai.Middleware.RequestLogger
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Yesod.Auth
import Yesod.Core.Types (loggerSet, Logger(Logger))
import Yesod.Default.Config
import Yesod.Default.Handlers

import Handlers
import qualified Scrapers
import Settings


-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp


-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Start the scraping thread
    _ <- forkIO $ Scrapers.main foundation

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    return $ (if development then logWare else id) app


-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager conduitManagerSettings
    s <- staticSite
    dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
    conf <- loader
    app <- makeApplication conf
    return (appPort conf, app)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
