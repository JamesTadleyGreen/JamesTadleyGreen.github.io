-- https://blog.ysndr.de/posts/internals/2020-03-22-built-with-hakyll-part-2/
module Site.Git(versionField, GitVersionContent(..)) where

import System.Exit (ExitCode(..))
import Hakyll
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import           System.Process


data GitVersionContent
  = Hash
  | Commit
  | Full
  deriving (Eq, Read)

instance Show GitVersionContent where
  show content =
    case content of
      Hash -> "%h"
      Commit -> "%h: %s"
      Full -> "%h: %s (%ai)"

-- Query information of a given file tracked with git
getGitVersion ::
     GitVersionContent -- Kind of information
  -> FilePath -- File to query information of
  -> IO String --
getGitVersion content path = do
  (status, stdout, _) <-
    readProcessWithExitCode
      "git"
      ["log", "-1", "--format=" ++ show content, "--", "src/" ++ path]
      ""
  return $
    case status of
      ExitSuccess -> trim stdout
      _ -> ""
  where
    trim = dropWhileEnd isSpace

-- Field that contains the latest commit hash that hash touched the current item.
versionField :: String -> GitVersionContent -> Context String
versionField name content =
  field name $ \item ->
    unsafeCompiler $ do
      let path = toFilePath $ itemIdentifier item
      getGitVersion content path
