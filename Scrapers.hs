module Scrapers where

import ClassyPrelude hiding (IOData(..))
import Prelude (head, read, tail, (!!))

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Logger
import Control.Monad.Reader hiding (forM, forM_)
import Data.Aeson
import Data.Char (isLower)
import Data.List (and)
import qualified Data.Map as Map
import Data.Text (breakOn, strip)
import Data.Text.IO (hGetContents, writeFile)
import Data.Time
import Data.Time.Clock.POSIX
import Database.Persist
import Network.HTTP.Client
import Network.URI
import System.IO (IOMode(..), withFile)
import System.Locale (defaultTimeLocale)
import Text.Regex.PCRE
import Text.XML hiding (writeFile)
import Text.XML.Cursor
import Text.XML.Selector

import Yesod.Core.Dispatch (fromPathPiece)

import Scraper

import Foundation
import Model
import Settings (PersistConf)
import Utils


type Scraper = ScraperA PersistConf


afterColon :: Text -> Text
afterColon s =
    case splitted of
        (_, value) | ": " `isPrefixOf` value -> drop 2 value
        _ -> error ("breakOn \": \" returned "++show splitted)
  where
    splitted = breakOn ": " $ unwords $ words s


readDay :: Text -> Day
readDay = readTime defaultTimeLocale "%d/%m/%Y" . unpack


cleanTitle :: Text -> Text
cleanTitle title
    | and (map (not . isLower) $ unpack title) = toUpper1 $ toLower title
    | otherwise = title


splitTitle :: Text -> (Text, Text)
splitTitle title =
    case mrSubList mr of
        (a:b:[]) -> (decodeUtf8 a, toUpper1 $ decodeUtf8 b)
        _ -> (title, "")
  where
    t = "(.+?)(?:\\s+[-–]+\\s+|[:.]\\s+)(.*)" :: Text
    regex = makeRegex (encodeUtf8 t :: ByteString) :: Regex
    mr = match regex $ encodeUtf8 title


scrapeITranslation :: URI -> InitiativeId -> Text -> Text -> Scraper Text
scrapeITranslation uri iid lang refWebsite = do
    maybeInitiative <- runDB $ get iid
    maybeTranslation <- runDB (getBy $ UniqueTranslation iid lang) >>=
                        return . fmap entityVal
    baseReq <- asks scraperBaseReq
    (_, docCursor) <- scrapeHtml baseReq uri
    let contentBoxes = docCursor $// byId "center" &/ byClass "contentBox"
        firstBox = contentBoxes!!0
        firstBoxStrings = firstBox $// strippedStrings
        items = contentBoxes!!4 $// element "li" &| node
        docPath = "/citizens-initiative/public/documents/"
        docLinks cursor = docLink $ node cursor
        docLink (NodeElement (Element _ attrs _)) =
            case lookup "href" attrs of
                Just href | isInfixOf docPath href ->
                    [tshow (parseURIReference' (unpack href) `relativeTo` uri)]
                _ -> []
        docLink _ = []
        initiative = Initiative
            (readDay $ afterColon $ firstBoxStrings!!5)
            (readDay $ afterColon $ firstBoxStrings!!3)
            (afterColon $ getText $ items!!0)
            (afterColon $ getText $ items!!1)
            (afterColon $ getText $ items!!2)
            (toLower $
                (firstBox $/ byClass "languagesAvailableBox" &// element "td"
                    &/ anyElement &/ content)!!0)
            (maybe "" initiativeOcsUrl maybeInitiative)
        (title, subtitle) = splitTitle $ cleanTitle $ firstBoxStrings!!1
        website = zeroOrOne refWebsite $ concat $ contentBoxes!!5 $/
                  byClass "subcontentText" &/ element "a" &| attribute "href"
        itranslation = ITranslation
            iid
            lang
            title
            subtitle
            (strip $ getText $ contentBoxes!!1 $/ byClass "subcontentText")
            (strip $ getText $ contentBoxes!!2 $/ byClass "subcontentText")
            (maybe website iTranslationWebsite maybeTranslation)
            (zeroOrOne "" $ contentBoxes!!6 $// docLinks)
            (zeroOrOne "" $ contentBoxes!!7 $// docLinks)
            (maybe "" iTranslationFacebook maybeTranslation)
            (maybe "" iTranslationTwitter maybeTranslation)
            (maybe "" iTranslationGoogle maybeTranslation)
            (maybe "" iTranslationYoutube maybeTranslation)

    runDB_ $ do
        when (maybeInitiative /= Just initiative) $ repsert iid initiative
        repsertUnique (UniqueTranslation iid lang) itranslation

    if initiativeRegistrationLanguage initiative == lang
        then return $ iTranslationWebsite itranslation
        else return ""


scrapeInitiatives :: Scraper ()
scrapeInitiatives = do
    let rootUri = $(parseURI' "http://ec.europa.eu/citizens-initiative/public/initiatives/ongoing")
        urlsWeWant = "^"++show rootUri++"/details/(\\d{4})/(\\d{6})/([a-z]+)$"
    baseReq <- asks scraperBaseReq
    (_, cursor) <- scrapeHtml baseReq rootUri
    let rows = cursor $// byClass "eci-table" &// element "tr"
    let lists = filter (not . null) $
                map (\row -> matchLinks rootUri row urlsWeWant) rows
    when (null lists) $
        $(logError) ("failed to extract URLs to scrape from "++tshow rootUri)
    forM_ lists $ \matches -> do
        -- ↓ this assumes that the registration language is first in the list
        let mr0 = head matches
            urlParts = mrSubList mr0
            i = pack (urlParts!!0 ++ urlParts!!1)
        case fromPathPiece i of
            Nothing -> $(logError) ("failed to read InitiativeId: "++i)
            Just iid -> scrapeInitiative iid mr0 (tail matches)
  where
    scrapeInitiative iid firstMR rest = do
        let uri = getURI firstMR
        refWebsite <- scrapeITranslation uri iid (getLang firstMR) ""
        when (null refWebsite) $ do
            $(logError) ("website URL not found in "++tshow uri)
        forM_ rest $ \mr -> do
            _ <- scrapeITranslation (getURI mr) iid (getLang mr) refWebsite
            liftIO $ threadDelay 15000000
    getURI = parseURIReference' . mrMatch
    getLang mr = pack $ (mrSubList mr)!!2


data MapData = MapData
    { mdTotal :: Text
    , mdCountries :: [CountryData]
    }

instance FromJSON MapData where
    parseJSON (Object o) = MapData <$>
                           o .: "total" <*>
                           o .: "countries"
    parseJSON _ = mempty

data CountryData = CountryData
    { countryCode :: Text
    , countryName :: Text
    , countryCount :: Text
    , countryThreshold :: Text
    , countryPercentage :: Text
    }

instance FromJSON CountryData where
    parseJSON (Object o) = CountryData <$>
                           o .: "code" <*>
                           o .: "name" <*>
                           o .: "count" <*>
                           o .: "threshold" <*>
                           o .: "percentage"
    parseJSON _ = mempty


scrapeMapData :: Entity Initiative -> Scraper ()
scrapeMapData e@(Entity _ Initiative{..}) = do
    let ocsUrl = unpack initiativeOcsUrl
    baseReq <- asks scraperBaseReq
    _ <- scrapeHtml (noRedirects baseReq) $ parseURIReference' (ocsUrl++"/map.do")
    value <- scrapeJson baseReq $ parseURIReference' (ocsUrl++"/mapdata.do")
    let mapdata = fromJSON' value
    if null $ mdCountries mapdata
        then return ()
        else saveMapData e mapdata


saveMapData :: Entity Initiative -> MapData -> Scraper ()
saveMapData (Entity iid Initiative{..}) MapData{..} = do
    (now, LocalTime{..}) <- liftIO getCurrentTimes
    let (today, time) = (localDay, round $ timeOfDayToTime localTimeOfDay)
        yesterday = addDays (-1) today
        dayZero = addDays (-1) initiativeStart
        mapFromOCS = Map.fromList $ map (\a -> (countryCode a, a)) mdCountries
    runDB_ $ do
        webCounters <- Map.fromList <$>
            map (\e -> (iWebCounterCountryCode $ entityVal e, e)) <$>
            selectList [IWebCounterInitiativeId ==. iid,
                        IWebCounterDay ==. today]
                       []
        webCountersYesterday <- Map.fromList <$>
            map (\c -> (iWebCounterCountryCode c, c)) <$>
            map entityVal <$>
            selectList [IWebCounterInitiativeId ==. iid,
                        IWebCounterDay ==. yesterday]
                       []
        l <- do
            counters <- selectList [ICounterInitiativeId ==. iid,
                                    ICounterCountryCode !=. ""]
                                   []
            let mapFromDB = Map.fromList $
                    map (\e -> (iCounterCountryCode $ entityVal e, e)) counters
            forM_ (Map.elems $ Map.difference mapFromDB mapFromOCS) $ \e -> do
                let country = iCounterCountryCode $ entityVal e
                $(logWarn) ("The OCS didn't return data for"++
                            " iid="++tshowKeyUnsafe iid++
                            " country="++country)
            l <- forM (Map.elems $ Map.difference mapFromOCS mapFromDB) $ \d -> do
                let c = ICounter iid (countryCode d) now Nothing 0 Nothing 0 0 0 0
                e <- insertByValue c
                return (e, d)
            return (l ++ Map.elems (Map.intersectionWith (,) mapFromDB mapFromOCS))
        counters <- forM l $ \(Entity counterId counter, CountryData{..}) -> do
            let maybeWebCountYesterday =
                    case lookup countryCode webCountersYesterday of
                        Just c | iWebCounterTime c > 84600 -> Just $ iWebCounterCount c
                        _ | yesterday == dayZero -> Just 0
                        _ -> Nothing
            let webCount = read $ unpack countryCount
            case lookup countryCode webCounters of
                Just (Entity key IWebCounter{..}) -> do
                    when (iWebCounterCount /= webCount ||
                          time > (iWebCounterTime + 300)) $
                        update key [IWebCounterTime =. time, IWebCounterCount =. webCount]
                Nothing -> do
                    _ <- insertUnique $ IWebCounter iid countryCode today time webCount
                    return ()
            let delta = fmap (webCount -) maybeWebCountYesterday
                total = webCount + (maybe 0 id $ iCounterPaper counter)
                quota = read $ unpack countryThreshold
                target = round (fromIntegral quota * 1.75 :: Double)
                targetPercentage = percent total target
            updateGet counterId [ICounterTimestamp =. now,
                                 ICounterToday =. delta,
                                 ICounterWeb =. webCount,
                                 ICounterTotal =. total,
                                 ICounterQuota =. quota,
                                 ICounterTarget =. target,
                                 ICounterTargetPercentage =. targetPercentage]
        _ <- insertUnique $ ICounter iid "" now Nothing 0 Nothing 0 0 0 0
        (Entity counterId _) <- getBy' $ UniqueCounter iid ""
        let total = sum (map iCounterTotal counters)
            target = sum (map iCounterTarget counters)
            targetPercentage = percent total target
        update counterId
            [ICounterTimestamp =. now,
             ICounterToday =. fmap sum (catJusts $ map iCounterToday counters),
             ICounterWeb =. sum (map iCounterWeb counters),
             ICounterTotal =. total,
             ICounterQuota =. sum (map iCounterQuota counters),
             ICounterTarget =. target,
             ICounterTargetPercentage =. targetPercentage]


scrapeCounters :: Scraper ()
scrapeCounters = do
    (_, now) <- liftIO $ getCurrentTimes
    initiatives <- runDB $ selectList [InitiativeDeadline >=. localDay now]
                                      [Asc InitiativeDeadline]
    when (null initiatives) $
        error "There are no ongoing initiatives in the DB."
    let (noOcsUrl, withOcsUrl) = partition (null . initiativeOcsUrl . entityVal) initiatives
    when (not $ null noOcsUrl) $
        $(logInfo) (tshow (length noOcsUrl)++" initiatives have no OCS URL")
    forM_ withOcsUrl $ \e -> do
        scrapeMapData e
        liftIO $ threadDelay 4000000


scraperEnvFromApp :: App -> Request -> ScraperEnv PersistConf
scraperEnvFromApp App{..} baseReq =
    ScraperEnv baseReq connPool httpManager appLogger persistConfig


main :: App -> IO ()
main app = do
    let headers = [("User-Agent","CitizensInitiativesScraper"),
                   ("Accept-Language","en")]
        baseReq = def{ requestHeaders = headers }
        env = scraperEnvFromApp app baseReq
        jar = createCookieJar []
    _ <- forkIO $ forever $ do
        printErrors $ do
            now <- round <$> utcTimeToPOSIXSeconds <$> getCurrentTime
            let anHourAgo = now - 3600 :: Int
            lastScrape <- withFile "scrapeInitiatives" ReadWriteMode $ \h ->
                maybe 0 id <$> readMay <$> hGetContents h
            when (lastScrape < anHourAgo) $ do
                printErrors $ runScraper jar env scrapeInitiatives
                writeFile "scrapeInitiatives" $ tshow now
        threadDelay 300000000
    forever $ do
        printErrors $ forever $ runScraper jar env scrapeCounters
        threadDelay 60000000
