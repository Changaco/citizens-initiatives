module Utils where

import ClassyPrelude

import Control.Monad.Trans.Control
import Data.Aeson
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.URI
import System.IO (hFlush)
import Text.Printf


fallback :: (Eq a, Monoid a) => a -> a -> a
fallback a b = if a == mempty then b else a


nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go Set.empty
  where go _ [] = []
        go s (x:xs) | Set.member x s = go s xs
                    | otherwise      = x : go (Set.insert x s) xs


printErrors :: (MonadIO m, MonadBaseControl IO m) => m () -> m ()
printErrors = handleAny (\e -> liftIO (print e >> hFlush stdout))


toUpper1 :: Text -> Text
toUpper1 t = T.toUpper (T.take 1 t) ++ T.drop 1 t


-- * i18n

plural :: (Num i, Eq i) => i -> Text -> Text -> Text
plural 1 a _ = a
plural _ _ b = b


-- * JSON

fromJSON' :: FromJSON a => Value -> a
fromJSON' v =
    case fromJSON v of
        Error s -> error s
        Success r -> r


-- * Maybe

catJusts :: [Maybe a] -> Maybe [a]
catJusts = foldr f (Just [])
  where
    f (Just x) (Just xs) = Just (x:xs)
    f _ _ = Nothing


forMM :: Monad m => [a] -> (a -> m (Maybe b)) -> m [b]
forMM a b = liftM catMaybes $ forM a b


-- * Percentages

percent :: (Integral a, Integral b) => a -> b -> Double
percent x y = fromIntegral x / fromIntegral y

showPercent :: Double -> String
showPercent p = printf "%.1f%%" (p * 100)

success :: Double -> Text
success p = if p >= 1 then "success" else ""


-- * Persistent

getBy' unique = do
    maybeEntity <- getBy unique
    maybe (error ("getBy returned Nothing for: "++show unique))
          return maybeEntity


-- | Like 'insertBy' but returns an 'Entity' instead of an 'Either'.
insertByValue val = insertBy val >>= return . either id (\key -> Entity key val)


repsertUnique unique val = do
    maybeEntity <- getBy unique
    case maybeEntity of
        Just (Entity key oldVal) -> do
            when (oldVal /= val) $ replace key val
            return key
        Nothing -> insert val


-- * Time

getCurrentTimes :: IO (UTCTime, LocalTime)
getCurrentTimes = do
    now <- getCurrentTime
    localZone <- getTimeZone now
    return (now, utcToLocalTime localZone now)


-- * URI

parseURI' :: Text -> Q Exp
parseURI' uri =
    case parseURI $ unpack uri of
        Just u -> dataToExpQ (const Nothing) u
        Nothing -> error ("failed to parse uri "++unpack uri)


parseURIReference' :: String -> URI
parseURIReference' url =
    case parseURIReference url of
        Just uri -> uri
        Nothing -> error ("failed to parse URL: "++url)
