{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Text.Blaze.Html (preEscapedToHtml)
import qualified CMark as C
import qualified Data.Text.IO as D

menuItems :: [(Route App, String)]
menuItems = [(HomeR, "Home"), (Page1R, "Page")]


getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Website"
        $(widgetFile "home")

getPage1R :: Handler Html
getPage1R =  defaultLayout [whamlet|Hello World!|]


getMarkdownFile :: String -> FilePath
getMarkdownFile title = "./markdown/" ++ title ++ ".md"

markdownToHtml :: Text -> Text
markdownToHtml m = C.commonmarkToHtml [C.optUnsafe] m

getBlogR :: String -> Handler Html
getBlogR title = do
    markdown <- liftIO $ D.readFile $ getMarkdownFile title
    let html = markdownToHtml markdown
    defaultLayout $ do
        setTitle "Title"
        addScript $ StaticR js_highlight_highlight_min_js
        addStylesheet $ StaticR js_highlight_styles_tokyo_night_dark_min_css
        toWidget $ preEscapedToHtml html
