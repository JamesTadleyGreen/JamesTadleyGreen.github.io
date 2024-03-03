--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Bifunctor (Bifunctor(bimap))
import Data.List (intersperse, intercalate)
import Data.Map as M
import Data.Maybe (maybeToList)
import Data.Monoid (mappend)
import Data.Text (Text, pack)
import Data.List.Split (splitOn)
import Debug.Trace
import Hakyll
import System.FilePath
import Text.Blaze.Html ((!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Pandoc.Class
import Text.Pandoc.Definition (Block(CodeBlock, Div, Null), Pandoc)
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options
  ( Extension(Ext_abbreviations, Ext_fenced_divs,
          Ext_inline_code_attributes, Ext_latex_macros, Ext_tex_math_dollars,
          Ext_tex_math_double_backslash, Ext_tex_math_single_backslash)
  , HTMLMathMethod(MathJax)
  , ReaderOptions(..)
  , WriterOptions(..)
  , extensionsFromList
  , readerExtensions
  )
import Text.Pandoc.Templates
import Text.Pandoc.Walk

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
    match "code/**" $ do
      route idRoute
      compile getResourceString
    match (Hakyll.fromList ["about.md"]) $ do
      route $ setExtension "html"
      compile $
        pandocCompiler' empty >>=
        loadAndApplyTemplate "templates/default.html" defaultContext >>=
        relativizeUrls
    tags <- buildTags "posts/**" (fromCapture "tags/*.html")
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
        pandocCompiler' undefined >>=
        loadAndApplyTemplate "templates/post.html" (postCtx tags) >>=
        loadAndApplyTemplate "templates/default.html" (postCtx tags) >>=
        relativizeUrls
    match "posts/**/*.md" $ do
      route $ setExtension "html"
      compile $ do
        snippets <-
          fileToSnippet <$> load "code/the_last_algorithms_course/1-intro.py"
        thisPostNum <- getPostNum <$> getResourceString
        (posts :: [Item String]) <- getIdentifiers "posts/**/*.md"
        let ctx =
              listField
                "posts"
                (postCtx tags <> multiPostCtx thisPostNum)
                (return posts) <>
              postCtx tags
        pandocCompiler' snippets >>=
          loadAndApplyTemplate "templates/post.html" ctx >>=
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
postCtx tags =
  tagsFieldWith getTags renderLink (mconcat . intersperse " #") "tags" tags `mappend`
  defaultContext

--------------------------------------------------------------------------------
renderLink :: String -> Maybe FilePath -> Maybe H.Html
renderLink _ Nothing = Nothing
renderLink tag (Just filePath) =
  Just $
  H.a Text.Blaze.Html.! A.class_ "tag" Text.Blaze.Html.!
  A.href (toValue $ toUrl filePath) $
  toHtml tag

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
pandocCompiler' :: M.Map Text Text -> Compiler (Item String)
pandocCompiler' snippets =
  pandocCompilerWithTransform
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
            , Ext_fenced_divs
            ]
      }
    defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocHighlightingStyle
      , writerHTMLMathMethod = MathJax ""
      , writerTableOfContents = True
      , writerNumberSections = True
      , writerTOCDepth = 2
      -- , writerTemplate = Just tocTemplate
      }
    (codeInclude snippets)

--------------------------------------------------------------------------------
fileToSnippet :: Item String -> M.Map Text Text
fileToSnippet item = M.fromList (Prelude.map (Data.Bifunctor.bimap pack pack) kvs)
  where
    snippets = splitOn "§" (itemBody item)
    kvs = Prelude.map (extractName . lines) snippets
    extractName s = (Prelude.head s, intercalate "\n" $ Prelude.tail s)

codeInclude :: M.Map Text Text -> Pandoc -> Pandoc
codeInclude snippets =
  walk $ \block ->
    case block of
      div@(Div (_, cs, _) _) ->
        if "code-include" `elem` cs
          then codeBlockFromDiv snippets div
          else block
      _ -> block

codeBlockFromDiv :: M.Map Text Text -> Block -> Block
codeBlockFromDiv snippets div@(Div (_, _, kvs) _) =
  let classes = "numberLines" : maybeToList (Prelude.lookup "lexer" kvs)
      content = Prelude.lookup "name" kvs >>= (`M.lookup` snippets)
   in maybe Null (CodeBlock ("", classes, [])) content
codeBlockFromDiv _ _ = Null

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
