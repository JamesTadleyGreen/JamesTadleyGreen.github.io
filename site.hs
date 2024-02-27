--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))
import Debug.Trace
import System.FilePath

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do makeItem $ styleToCss pandocCodeStyle
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
              constField "title" title
                `mappend` listField "posts" (postCtx tags) (return posts)
                `mappend` defaultContext

        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
    match "posts/*.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
          >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
          >>= relativizeUrls
    match "posts/**/*.md" $ do
      route $ setExtension "html"
      compile $ do
        thisPostNum <- getPostNum <$> getResourceString
        (posts :: [Item String]) <- getIdentifiers "posts/**/*.md"
        let ctx =
              listField "posts" (postCtx tags `mappend` multiPostCtx thisPostNum) (return posts)
              `mappend` defaultContext
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/post.html" ctx
          >>= loadAndApplyTemplate "templates/multi-post.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**/*.md"
        let archiveCtx =
              listField "posts" defaultContext (return posts)
                `mappend` constField "title" "Archives"
                `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx
          >>= relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**/*"
        let indexCtx =
              listField "posts" defaultContext (return posts) `mappend` defaultContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = tagsField "tags" tags `mappend` defaultContext

--------------------------------------------------------------------------------
multiPostCtx :: String -> Context String
multiPostCtx currentPostNum = field "postNum" (return . getPostNum) <> boolField "isCurrentPost"  ((== currentPostNum) . getPostNum)

getPostNum :: Item a -> String
getPostNum = takeWhile (/='-') . takeBaseName . toFilePath . itemIdentifier
--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {destinationDirectory = "docs"}

--------------------------------------------------------------------------------
pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions {writerHighlightStyle = Just pandocCodeStyle}
--------------------------------------------------------------------------------
getIdentifiers :: Pattern -> Compiler [Item String]
getIdentifiers pattern = do
    identifiers <- getMatches pattern
    return [Item identifier "" | identifier <- identifiers]
