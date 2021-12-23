{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Options.Applicative
import Parser.Tagless.Closed
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

data Args = Args {argsFileName :: !FilePath, argsMethod :: !Text}

runExpr :: (Text -> Either Text (Dynamic Eval)) -> Text -> IO ()
runExpr f input = case f input of
  Right d -> case fromDyn @Eval @Integer d of
    Just a -> print a
    Nothing -> case fromDyn @Eval @Bool d of
      Just a -> print a
      Nothing -> print "Could not evaluate expression fully."
  Left e -> print e

args :: Parser Args
args = Args
  <$> strArgument (metavar "FILENAME" <> help "The file we want to parse.")
  <*> strOption
     ( short 'm'
    <> long "method"
    <> metavar "METHOD"
    <> showDefault
    <> value "mul_pass"
    <> help "The parse strategy we want to try. Should be one of \
            \\"mul_pass\" or \"mem_cons\"."
     )

run :: Args -> IO ()
run args = do
  handle <- openFile (argsFileName args) ReadMode
  contents <- hGetContents handle
  case argsMethod args of
    "mul_pass" -> runExpr runMulPass contents
    "mem_cons" -> runExpr runMemCons contents
    _          -> error "Encountered an invalid parsing strategy."

main :: IO ()
main = run =<< execParser opts
 where
  opts = info (args <**> helper)
    ( fullDesc
   <> progDesc "Different parsing strategies using initial encoding"
   <> header "Initial encoding parsing"
    )
