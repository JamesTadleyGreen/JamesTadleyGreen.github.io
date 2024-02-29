--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid (mappend)
import Debug.Trace
import Hakyll
import System.FilePath
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Templates
import Text.Pandoc.Class
import Text.Pandoc.Options
  ( Extension(Ext_abbreviations, Ext_inline_code_attributes,
          Ext_latex_macros, Ext_tex_math_dollars,
          Ext_tex_math_double_backslash, Ext_tex_math_single_backslash)
  , HTMLMathMethod(MathJax)
  , ReaderOptions(..)
  , WriterOptions(..)
  , extensionsFromList
  , readerExtensions
  )

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyllWith config $ do
    match "images/*" $ do
      route idRoute
      compile copyFileCompiler
    create ["css/syntax.css"] $ do
      route idRoute
      compile $ do makeItem $ styleToCss pandocHighlightingStyle
    match "css/*" $ do
      route idRoute
      compile compressCssCompiler
    match (fromList ["about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler' >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx =
              constField "title" title `mappend`
              listField "posts" (postCtx tags) (return posts) `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/tag.html" ctx >>=
          loadAndApplyTemplate "templates/default.html" ctx >>=
          relativizeUrls
    match "posts/*.md" $ do
      route $ setExtension "html"
      compile $
        pandocCompiler' >>=
        loadAndApplyTemplate "templates/post.html" (postCtx tags) >>=
        loadAndApplyTemplate "templates/default.html" (postCtx tags) >>=
        relativizeUrls
    match "posts/**/*.md" $ do
      route $ setExtension "html"
      compile $ do
        thisPostNum <- getPostNum <$> getResourceString
        (posts :: [Item String]) <- getIdentifiers "posts/**/*.md"
        let ctx =
              listField
                "posts"
                (postCtx tags `mappend` multiPostCtx thisPostNum)
                (return posts) `mappend`
              defaultContext
        pandocCompiler' >>= loadAndApplyTemplate "templates/post.html" ctx >>=
          loadAndApplyTemplate "templates/multi-post.html" ctx >>=
          loadAndApplyTemplate "templates/default.html" ctx >>=
          relativizeUrls
    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**/*.md"
        let archiveCtx =
              listField "posts" defaultContext (return posts) `mappend`
              constField "title" "Archives" `mappend`
              defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
          loadAndApplyTemplate "templates/default.html" archiveCtx >>=
          relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/**/*"
        let indexCtx =
              listField "posts" defaultContext (return posts) `mappend`
              defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Tags -> Context String
postCtx tags = tagsField "tags" tags `mappend` defaultContext

--------------------------------------------------------------------------------
multiPostCtx :: String -> Context String
multiPostCtx currentPostNum =
  field "postNum" (return . getPostNum) <>
  boolField "isCurrentPost" ((== currentPostNum) . getPostNum)

getPostNum :: Item a -> String
getPostNum = takeWhile (/= '-') . takeBaseName . toFilePath . itemIdentifier

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {destinationDirectory = "docs"}

--------------------------------------------------------------------------------
pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWith
    defaultHakyllReaderOptions
      { readerExtensions =
          readerExtensions defaultHakyllReaderOptions <>
          extensionsFromList
            [ Ext_tex_math_single_backslash -- TeX math btw (..) [..]
            , Ext_tex_math_double_backslash -- TeX math btw \(..\) \[..\]
            , Ext_tex_math_dollars -- TeX math between $..$ or $$..$$
            , Ext_latex_macros -- Parse LaTeX macro definitions (for math only)
            , Ext_inline_code_attributes -- Ext_inline_code_attributes
            , Ext_abbreviations -- PHP markdown extra abbreviation definitions
            ]
      }
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocHighlightingStyle
      , writerHTMLMathMethod = MathJax ""
      , writerTableOfContents = True
      , writerNumberSections  = True
      , writerTOCDepth        = 2
      -- , writerTemplate = Just tocTemplate
      }

--------------------------------------------------------------------------------
-- tocTemplate = do
--         res <- getTemplate "templates/toc.html" >>= runWithDefaultPartials . compileTemplate "templates/toc.html"
--         case res of
--             Left e   -> undefined
--             Right t  -> return t
--------------------------------------------------------------------------------
pandocHighlightingStyle :: Style
pandocHighlightingStyle = breezeDark

--------------------------------------------------------------------------------
getIdentifiers :: Pattern -> Compiler [Item String]
getIdentifiers pattern = do
  identifiers <- getMatches pattern
  return [Item identifier "" | identifier <- identifiers]
