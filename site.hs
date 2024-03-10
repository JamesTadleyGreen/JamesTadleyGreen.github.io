{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Site.Compiler (pandocCompiler')
import Site.Context (getPostNum, multiPostCtx, postCtx)
import Site.Git
import Site.Snippet (codeInclude, fileToSnippet, pandocHighlightingStyle)

import Data.Map as M
import Data.Text (Text, pack)
import System.Directory (doesFileExist)
import Hakyll
import System.FilePath.Posix
import Text.Pandoc.Highlighting (styleToCss)

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
        posts <- loadAll pattern
        let ctx =
              constField "title" title <>
              listField "posts" (postCtx tags) (return posts) <> defaultContext
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
        codeFile <- getCodeFile <$> getResourceString
        snippets <- fileToSnippet <$> maybeLoad codeFile
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
        posts <- loadAll "posts/**"
        let archiveCtx =
              listField "posts" (defaultContext <> gitFields) (return posts) <>
              constField "title" "Archives" <> defaultContext
        makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
          loadAndApplyTemplate "templates/default.html" archiveCtx >>=
          relativizeUrls
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- loadAll "posts/**"
        let indexCtx =
              listField "posts" (defaultContext <> gitFields) (return posts) <>
              gitFields <> defaultContext
        getResourceBody >>= applyAsTemplate indexCtx >>=
          loadAndApplyTemplate "templates/default.html" indexCtx >>=
          relativizeUrls
    match "templates/**" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration {destinationDirectory = "docs"}

--------------------------------------------------------------------------------
getIdentifiers :: Pattern -> Compiler [Item String]
getIdentifiers pattern = do
  identifiers <- getMatches pattern
  return [Item identifier "" | identifier <- identifiers]

--------------------------------------------------------------------------------
getCodeFile :: Item a -> Identifier
getCodeFile item =
  fromFilePath $ r (replaceExtension (toFilePath (itemIdentifier item)) ".py")
  where
    r path = joinPath $ ("code/" :: FilePath) : tail (splitPath path)

--------------------------------------------------------------------------------
maybeLoad :: Identifier -> Compiler (Item String)
maybeLoad identifier = do
    let filePath = toFilePath identifier
    exists <- unsafeCompiler $ doesFileExist filePath
    if exists
        then makeItem =<< unsafeCompiler (readFile filePath)
        else pure (Item identifier "")
