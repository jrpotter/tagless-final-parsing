{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Parser as P
import qualified Text.Megaparsec as M

import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Options.Applicative
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

data Args = Args
  { argsFileName :: !FilePath
  , argsMethod :: !Text
  }

args :: Parser Args
args = Args
  <$> strArgument (metavar "FILENAME" <> help "The file we want to parse.")
  <*> strOption
     ( short 'm'
    <> long "method"
    <> metavar "METHOD"
    <> showDefault
    <> value "naive"
    <> help "The parse strategy we want to try. Should be one of \
            \\"naive\", \"mul_pass\", or \"mem_cons\"."
     )

run :: Args -> IO ()
run args = do
  let method = case argsMethod args of
                 "naive"    -> P.naiveExpr
                 "mul_pass" -> P.mulPassExpr
                 "mem_cons" -> P.memConsExpr
                 _          -> error "Encountered an invalid parsing strategy."
  handle <- openFile (argsFileName args) ReadMode
  contents <- hGetContents handle
  case M.parse (method <* M.eof) (argsFileName args) contents of
    Left e -> print $ M.errorBundlePretty e
    Right a -> print $ P.eval a

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Different parsing strategies using initial encoding"
     <> header "Initial encoding parsing"
      )
