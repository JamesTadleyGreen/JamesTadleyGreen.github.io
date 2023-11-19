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
import Data.Text.Conversions
import Database.Persist.Sqlite

menuItems :: [(Route App, String)]
menuItems = [(BlogR "test", "Blog"), (CVR , "CV")]

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Website"
        $(widgetFile "home")


getMarkdownFile :: String -> String -> FilePath
getMarkdownFile folder title = "./markdown/" ++ folder ++ "/" ++ title ++ ".md"

markdownToHtml :: Text -> Text
markdownToHtml = C.commonmarkToHtml [C.optUnsafe]

getBlogR :: String -> Handler Html
getBlogR title = do
    markdown <- liftIO $ D.readFile $ getMarkdownFile "blog" title
    let html = markdownToHtml markdown
    defaultLayout $ do
        setTitle "Title"
        addScript $ StaticR js_highlight_highlight_min_js
        addStylesheet $ StaticR js_highlight_styles_tokyo_night_dark_min_css
        addScriptRemote("https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
        toWidget $ preEscapedToHtml html

-- randomBlogPost :: Route App
-- randomBlogPost = do
--     contents <- getDirectoryContents "./markdown/blog/"
--     let post = head contents
--     BlogR post

getCVR :: Handler Html
getCVR = do
    --cvItems :: DB [Entity CvHeaders]
    cvItems <- runDB $ selectList [] [Asc CvHeadersTitle]
    defaultLayout $ do
        setTitle "CV"
        $(widgetFile "cv")


getCVDownloadR :: Handler Html
getCVDownloadR = do
    defaultLayout $ do
        setTitle "CV"
        sendFile "application/pdf" "static/files/James_Green_CV.pdf"


getCVPostR :: String -> Handler Html
getCVPostR title = do
    markdown <- liftIO $ D.readFile $ getMarkdownFile "cv" title
    let html = markdownToHtml markdown
    defaultLayout $ do
        setTitle "CV"
        addScript $ StaticR js_highlight_highlight_min_js
        addStylesheet $ StaticR js_highlight_styles_tokyo_night_dark_min_css
        toWidget $ preEscapedToHtml html
