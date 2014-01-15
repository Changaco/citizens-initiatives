module Handlers where

import Import

import Data.Text (breakOn)
import Data.Time

import Widgets


hasSocialLinks :: ITranslation -> Bool
hasSocialLinks ITranslation{..} = (not $ null iTranslationFacebook)
                               || (not $ null iTranslationTwitter)
                               || (not $ null iTranslationGoogle)
                               || (not $ null iTranslationYoutube)


getInitiativeTranslation :: Entity Initiative -> Handler ITranslation
getInitiativeTranslation (Entity iid i) = do
    langs <- nubOrd <$> (++["en"]) <$> map (fst . breakOn "-") <$> languages
    go $ take 4 langs
  where
    go (lang:rest) = do
        maybeTranslation <- runDB $ getBy $ UniqueTranslation iid lang
        maybe (go rest) (return . entityVal) maybeTranslation
    go [] = do
        e <- runDB $ getBy404 $ UniqueTranslation iid $ initiativeRegistrationLanguage i
        return $ entityVal e


getTotalCounter :: InitiativeId -> Handler (Int, Int)
getTotalCounter iid = do
    maybeCounter <- runDB $ getBy (UniqueCounter iid "")
    case maybeCounter of
        Just (Entity _ c) -> return (iCounterTotal c, iCounterTarget c)
        Nothing -> return (0, -1)


getHomeR :: Handler Html
getHomeR = do
    (_, now) <- liftIO $ getCurrentTimes
    let today = localDay now
    initiatives <- do
        l <- runDB $ selectList [InitiativeDeadline >=. today]
                                [Asc InitiativeDeadline]
        forM l $ \e@(Entity iid i) -> do
            (signatures, target) <- getTotalCounter iid
            translation <- getInitiativeTranslation e
            let daysLeft = fromInteger $ diffDays (initiativeDeadline i) today
            return (e, signatures, target, translation, daysLeft)
    defaultLayout $ do
        setTitleI MsgSiteTitle
        $(widgetFile "home")


getInitiativeR :: InitiativeId -> Handler Html
getInitiativeR iid = do
    (_, now) <- liftIO $ getCurrentTimes
    let today = localDay now
    initiative@Initiative{..} <- runDB $ get404 iid
    iTranslation <- getInitiativeTranslation $ Entity iid initiative
    otherTranslations <- runDB $ map (iTranslationLang . entityVal) <$>
        selectList [ITranslationInitiativeId ==. iid,
                    ITranslationLang !=. iTranslationLang iTranslation]
                   [Asc ITranslationLang]
    app <- getYesod
    counters <- runDB $ map entityVal <$>
        selectList [ICounterInitiativeId ==. iid] [Desc ICounterTargetPercentage]
    (signatures, target) <- getTotalCounter iid
    let daysLeft = fromInteger $ diffDays initiativeDeadline today
    defaultLayout $ do
        let ITranslation{..} = iTranslation
        setTitle $ toHtml $ iTranslationTitle
        $(widgetFile "initiative")


getAboutR :: Handler Html
getAboutR =
    defaultLayout $ do
        setTitleI MsgAbout
        $(widgetFile "about")
