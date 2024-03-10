-- https://blog.ysndr.de/posts/internals/2020-03-22-built-with-hakyll-part-2/
-- Modified slightly
module Site.Git
  ( gitFields
  ) where

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Hakyll
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

formatDate :: String -> String
formatDate = take 10


getGitVersion :: FilePath -> IO [String]
getGitVersion path = do
  (status, stdout, a) <-
    readProcessWithExitCode "git" ["log", "-1", "--format=%h|%s|%ai", path] ""
  let [hash, message, date] = splitOn "|" stdout
  return $
    case status of
      ExitSuccess -> [hash, message, formatDate date]
      _ -> []

gitFields :: Context String
gitFields =
  mconcat [getGit "gitHash" 0, getGit "gitMessage" 1, getGit "date" 2]
  where
    getGit name idx =
      field name $ \item ->
        unsafeCompiler $ do
          let path = toFilePath $ itemIdentifier item
          getGitVersion path <&> (!! idx)
