{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

menuItems = [(HomeR, "Home"), (Page1R, "Page")]

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Website"
        $(widgetFile "home")

getPage1R :: Handler Html
getPage1R =  defaultLayout [whamlet|Hello World!|]

