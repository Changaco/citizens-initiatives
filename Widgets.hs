module Widgets where

import Import


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


signButton :: Text -> Widget
signButton ocsUrl = [whamlet|
$if not $ null ocsUrl
    <a class="btn btn-success" href="#{ocsUrl}">
        _{MsgSignOnline}
$else
    <span class="btn-group" data-toggle="tooltip" data-placement="bottom"
          title="_{MsgSignOnlineNotReady}">
        <a class="btn btn-success disabled">
            _{MsgSignOnline}
|]
