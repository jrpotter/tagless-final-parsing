{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Options.Applicative as O

import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Options.Applicative ((<**>))
import Parser.Final
import Parser.Utils
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

-- ========================================
-- Arguments
-- ========================================

data Args = Args {argsFileName :: !FilePath, argsMethod :: !Text}

args :: O.Parser Args
args = Args
  <$> O.strArgument
      ( O.metavar "FILENAME" <> O.help "The file we want to parse."
      )
  <*> O.strOption
      ( O.short 'm'
     <> O.long "method"
     <> O.metavar "METHOD"
     <> O.showDefault
     <> O.value "single"
     <> O.help "The parse strategy we want to try. Should be one of 'single' \
               \or 'strict'."
      )

-- ========================================
-- Main
-- ========================================

runExpr :: Parser (Dynamic Eval) -> Text -> IO ()
runExpr p input = case runParser p input of
  Left e -> print e
  Right d -> case fromDyn @Eval @Integer d of
    Just a -> print a
    Nothing -> case fromDyn @Eval @Bool d of
      Just a -> print a
      Nothing -> print "Could not evaluate expression fully."

run :: Args -> IO ()
run args = do
  handle <- openFile (argsFileName args) ReadMode
  contents <- hGetContents handle
  case argsMethod args of
    "single" -> runExpr parseSingle contents
    "strict" -> runExpr parseStrict contents
    _          -> error "Encountered an invalid parsing strategy."

main :: IO ()
main = run =<< O.execParser opts
 where
  opts = O.info (args <**> O.helper)
    ( O.fullDesc
   <> O.progDesc "Different parsing strategies using initial encoding"
   <> O.header "Initial encoding parsing"
    )
