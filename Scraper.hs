module Scraper
    ( ScraperA
    , ScraperEnv(..)
    , runScraper
    , runDB
    , runDB_
    , addHeader
    , noRedirects
    , scrapeHttp
    , scrapeHtml
    , scrapeJson
    , GetText(..)
    , matchLinks
    , strippedStrings
    , zeroOrOne
    ) where

import ClassyPrelude

import Control.Failure
import Control.Monad.Base
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Conduit
import Data.Conduit.Attoparsec (sinkParser)
import Data.Text (strip)
import Database.Persist.Class (PersistConfig(..))
import Language.Haskell.TH.Syntax (Loc(..))
import Network.HTTP.Client.Internal (setUri)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.URI
import System.Log.FastLogger (LogStr, toLogStr)
import qualified Text.HTML.DOM
import Text.Regex.PCRE
import Text.XML
import Text.XML.Cursor
import Yesod.Core.Types (Logger(..), loggerPutStr)

import Settings.Development (development)


data ScraperEnv pc = ScraperEnv
    { scraperBaseReq :: Request
    , scraperConnPool :: PersistConfigPool pc
    , scraperHttpManager :: Manager
    , scraperLogger :: Logger
    , scraperPersistConfig :: pc
    }

newtype ScraperA pc a = Scraper (StateT CookieJar (ReaderT (ScraperEnv pc) (ResourceT IO)) a)
    deriving (Applicative, Functor, Monad, MonadBase IO, MonadIO, MonadResource,
              MonadThrow, MonadUnsafeIO)

instance MonadBaseControl IO (ScraperA pc) where
    newtype StM (ScraperA pc) a = StMS (ScraperA pc a)
    liftBaseWith f = liftIO $ f (return . StMS)
    restoreM (StMS s) = s

instance Exception e => Failure e (ScraperA pc) where
    failure = liftIO . failure

instance MonadReader (ScraperA pc) where
    type EnvType (ScraperA pc) = ScraperEnv pc
    ask = Scraper ask
    local a (Scraper s) = Scraper $ local a s

instance MonadState (ScraperA pc) where
    type StateType (ScraperA pc) = CookieJar
    get = Scraper get
    put = Scraper . put

instance MonadLogger (ScraperA pc) where
    monadLoggerLog loc src level msg
        | development || level == LevelWarn || level == LevelError = do
            logger <- asks scraperLogger
            now <- liftIO $ loggerDate logger
            let logStr = toLogStr now ++
                        " [" ++
                            toLogStr (showLevel level) ++
                            (if null src then "" else "#" ++ toLogStr src) ++
                        "] " ++
                        toLogStr msg ++
                        " @(" ++ showFileLocation loc ++ ")\n"
            liftIO $ loggerPutStr logger logStr
        | otherwise = return ()

showLevel :: LogLevel -> String
showLevel (LevelOther t) = unpack t
showLevel level = drop 5 $ show level

showFileLocation :: Loc -> LogStr
showFileLocation loc =
  (toLogStr $ loc_package loc) ++ ":" ++
  (toLogStr $ stripPrefix' "./" $ loc_filename loc) ++ ":" ++
  (toLogStr $ show line) ++ ":" ++
  (toLogStr $ show char)
  where
    (line, char) = loc_start loc

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' prefix a = maybe a id $ stripPrefix prefix a


runScraper :: CookieJar -> ScraperEnv pc -> ScraperA pc a -> IO a
runScraper jar env (Scraper a) =
    runResourceT (runReaderT (evalStateT a jar) env)


runDB :: (PersistConfig pc) => PersistConfigBackend pc (ScraperA pc) a -> ScraperA pc a
runDB a = do
    env <- ask
    runPool (scraperPersistConfig env) a (scraperConnPool env)


runDB_ :: (PersistConfig pc) => PersistConfigBackend pc (ScraperA pc) a -> ScraperA pc ()
runDB_ a = runDB a >> return ()


addHeader :: Header -> Request -> Request
addHeader header req = req{ requestHeaders = header : requestHeaders req }


noRedirects :: Request -> Request
noRedirects req = req{ redirectCount = 0, checkStatus = \_ _ _ -> Nothing }


scrapeHttp :: Request -> URI -> ScraperA pc (Response (ResumableSource (ScraperA pc) ByteString))
scrapeHttp baseReq uri = do
    $(logInfo) ("Sending request for "++tshow uri)
    cookies <- get
    req <- setUri baseReq{ cookieJar = Just cookies } uri
    manager <- asks scraperHttpManager
    response <- http req manager
    modify (++ responseCookieJar response)
    return response


scrapeHtml :: Request -> URI -> ScraperA pc (Document, Cursor)
scrapeHtml baseReq uri = do
    response <- scrapeHttp (addHeader ("Accept",accept) baseReq) uri
    doc <- responseBody response $$+- Text.HTML.DOM.sinkDoc
    return (doc, fromDocument doc)
  where
    accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"


scrapeJson :: Request -> URI -> ScraperA pc Value
scrapeJson baseReq uri = do
    response <- scrapeHttp (addHeader ("Accept",accept) baseReq) uri
    responseBody response $$+- sinkParser json
  where
    accept = "application/json, text/javascript, */*; q=0.01"


class GetText elem where
    getText :: elem -> Text

instance GetText Node where
    getText (NodeElement (Element _ attrs [])) = maybe "" id $ lookup "alt" attrs
    getText (NodeElement (Element _ _ cs)) = (concatMap getText) cs
    getText (NodeContent txt) = txt
    getText _ = ""

instance GetText Cursor where
    getText = getText . node

instance (GetText a) => GetText [a] where
    getText = concatMap getText


matchLinks :: URI -> Cursor -> String -> [MatchResult String]
matchLinks uri cursor regex =
    let hrefs = cursor $// element "a" >=> attribute "href"
        uris = map (`relativeTo` uri) $ mapMaybe (parseURIReference . unpack) hrefs
        r = makeRegex regex :: Regex
    in  filter (not . null . mrSubList) $ map (match r . show) uris


strippedStrings :: Cursor -> [Text]
strippedStrings = filter (not . null) . map strip . content


zeroOrOne :: Show a => a -> [a] -> a
zeroOrOne a [] = a
zeroOrOne _ (a:[]) = a
zeroOrOne _ l = error ("expected a list of zero or one item but got "++show l)
