module Main where

import qualified Text.Megaparsec as M

import Closed.Parser
import Data.Text.IO (hGetContents)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

-- ========================================
-- Main
-- ========================================

main :: IO ()
main = do
  [fileName] <- getArgs
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  case M.parse expr fileName contents of
    Left e -> print $ M.errorBundlePretty e
    Right a -> print (fromDyn a :: Maybe (Eval Integer))
