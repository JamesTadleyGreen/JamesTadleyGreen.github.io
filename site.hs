--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions (..), WriterOptions (..))

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
    match "posts/*" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler'
          >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
          >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
          >>= relativizeUrls
    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
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
        posts <- recentFirst =<< loadAll "posts/*"
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
