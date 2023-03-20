{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Yesod.Markdown (markdownToHtmlTrusted, markdownToHtml, markdownFromFile, markdownToHtmlWithExtensions, Markdown)
import Text.Pandoc.Extensions (githubMarkdownExtensions)
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Julius (RawJS (..))


pandocCodeStyle :: Style
pandocCodeStyle = breezeDark


data Date =
    Date {
        year   :: Int,
        month  :: Int,
        day    :: Int
    }
    deriving (Eq,Ord,Typeable)

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

getBlogR :: String -> Handler Html
getBlogR title = do
    content <- liftIO $ fmap (markdownToHtmlWithExtensions githubMarkdownExtensions)
        $ markdownFromFile $ getMarkdownFile title
    case content of
        Left _ -> defaultLayout $  [whamlet|<p>error</p>|]
        Right html -> defaultLayout $ [whamlet|<div class="content">#{html}</div>|]
