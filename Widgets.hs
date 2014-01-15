module Widgets where

import Import


daysLeftWidget :: Int -> Widget
daysLeftWidget daysLeft | daysLeft < 0 = [whamlet||]
daysLeftWidget daysLeft = [whamlet|
<p>
    <span class="days-left alert alert-#{styl}">
        <i class="fa fa-clock-o"></i> _{msg}
|]
    where msg = if daysLeft == 0 then MsgLastDay else MsgDaysLeft daysLeft
          styl :: Text
          styl = case () of
                     _ | daysLeft == 0 -> "danger"
                     _ | daysLeft <= 7 -> "warning"
                     _ -> "info"


progressBar :: Double -> Widget
progressBar p = [whamlet|
<div class="progress">
    <div class="progress-bar" style="width: #{s};">
        <span class="sr-only">#{s}
|]
    where s = showPercent p


signaturesWidget :: Int -> Int -> Widget
signaturesWidget x y | x == 0 || y == 0 = [whamlet||]
signaturesWidget signatures target = [whamlet|
<div class="signatures-widget">
    ^{progressBar $ percent signatures target}
    <p>_{MsgSignaturesCollected signatures}
|]


signButton :: Text -> Int -> Widget
signButton ocsUrl daysLeft = [whamlet|
$maybe msg <- maybeDisabled
    <span class="btn-group" data-toggle="tooltip" data-placement="bottom"
          title="_{msg}">
        <a class="btn btn-success disabled">
            _{MsgSignOnline}
$nothing
    <a class="btn btn-success" href="#{ocsUrl}">
        _{MsgSignOnline}
|]
  where
    maybeDisabled = case () of
        _ | null ocsUrl -> Just MsgSignOnlineNotReady
        _ | daysLeft < 0 -> Just MsgInitiativeEnded
        _ -> Nothing


socialLink :: Text -> Text -> Text -> Widget
socialLink href code content = [whamlet|
$if not $ null href
    <a href="#{href}">
        <i class="fa fa-lg fa-#{code}"></i>
        <span class="sr-only"> #{content}
|]


socialLinks :: ITranslation -> Widget
socialLinks ITranslation{..} = [whamlet|
^{socialLink iTranslationFacebook "facebook-square" "Facebook"}
^{socialLink iTranslationTwitter "twitter" "Twitter"}
^{socialLink iTranslationGoogle "google-plus" "Google+"}
^{socialLink iTranslationYoutube "youtube" "Youtube"}
|]
