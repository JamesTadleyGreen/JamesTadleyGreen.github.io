{-# LANGUAGE OverloadedStrings #-}

module Site.Context
  ( postCtx
  , multiPostCtx
  , getPostNum
  ) where

import Site.Git (versionField, GitVersionContent(Hash))


import Data.List (intercalate, intersperse)
import Hakyll
import System.FilePath (takeBaseName)
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

renderLink :: String -> Maybe FilePath -> Maybe H.Html
renderLink _ Nothing = Nothing
renderLink tag (Just filePath) =
  Just $
  H.a Text.Blaze.Html.! A.class_ "tag" Text.Blaze.Html.!
  A.href (toValue $ toUrl filePath) $
  toHtml tag

postCtx :: Tags -> Context String
postCtx tags =
  tagsFieldWith getTags renderLink (mconcat . intersperse " #") "tags" tags <>
  -- versionField "gitHash" Hash <>
  constField "gitHash" "123" <>
  constField "readingTime" "5" <>
  defaultContext

multiPostCtx :: String -> Context String
multiPostCtx currentPostNum =
  field "postNum" (return . getPostNum) <>
  boolField "isCurrentPost" ((== currentPostNum) . getPostNum)

getPostNum :: Item a -> String
getPostNum = takeWhile (/= '-') . takeBaseName . toFilePath . itemIdentifier
