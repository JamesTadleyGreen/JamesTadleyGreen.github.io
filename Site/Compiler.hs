{-# LANGUAGE OverloadedStrings #-}

module Site.Compiler
  ( pandocCompiler'
  ) where

import Site.Snippet (codeInclude, pandocHighlightingStyle)

import qualified Data.Map as M
import Data.Text (Text)
import Hakyll
import Text.Pandoc.Options
  ( Extension(..)
  , HTMLMathMethod(MathJax)
  , ReaderOptions(..)
  , WriterOptions(..)
  , extensionsFromList
  , readerExtensions
  )

pandocCompiler' :: M.Map Text (Int, Text) -> Compiler (Item String)
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
