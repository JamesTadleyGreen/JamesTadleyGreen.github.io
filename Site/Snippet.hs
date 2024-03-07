{-# LANGUAGE OverloadedStrings #-}

module Site.Snippet
  ( fileToSnippet
  , codeInclude
  , pandocHighlightingStyle
  ) where

import Site.Git (GitVersionContent(Hash), versionField)

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.Text (Text, pack)
import Hakyll (Item, itemBody)
import Text.Pandoc.Definition (Block(CodeBlock, Div, Null), Pandoc)
import Text.Pandoc.Highlighting (Style, breezeDark)
import Text.Pandoc.Walk (walk)

fileToSnippet :: Item String -> Map Text (Int, Text)
fileToSnippet item = fromList (map snd kvs)
  where
    snippets = splitOn "§" (itemBody item)
    kvs =
      scanl
        (\(lineNumber, (_, (_, _))) ->
           extractNameAndLineNumber lineNumber . lines)
        (0, ("", (0, "")))
        snippets
    extractNameAndLineNumber lineNumber s =
      ( lineNumber + length s - 1
      , ( pack $ head s
        , (lineNumber + 2, pack $ intercalate "\n" $ tail $ init s)))

codeInclude :: Map Text (Int, Text) -> Pandoc -> Pandoc
codeInclude snippets =
  walk $ \block ->
    case block of
      div@(Div (_, cs, _) _) ->
        if "code-include" `elem` cs
          then codeBlockFromDiv snippets div
          else block
      _ -> block

codeBlockFromDiv :: Map Text (Int, Text) -> Block -> Block
codeBlockFromDiv snippets div@(Div (_, _, kvs) _) =
  let snippet = lookup "name" kvs >>= (`M.lookup` snippets)
      classes = "numberLines" : maybeToList (lookup "lexer" kvs)
   in case snippet of
        Nothing -> Null
        Just (lineNumber, content) ->
          CodeBlock
            ("", classes, [("startFrom", pack $ show lineNumber)])
            content
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
