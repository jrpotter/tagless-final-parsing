{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Applicative as O
import qualified Text.Megaparsec as M

import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Options.Applicative ((<**>))
import Parser.Initial
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
     <> O.value "naive"
     <> O.help "The parse strategy we want to try. Should be one of 'naive', \
               \'single', 'strict', or 'gadt'."
      )

-- ========================================
-- Main
-- ========================================

run :: Args -> IO ()
run args = do
  handle <- openFile (argsFileName args) ReadMode
  input <- hGetContents handle
  case argsMethod args of
    "naive"  -> runExpr parseNaive input
    "single" -> runExpr parseSingle input
    "strict" -> runExpr parseStrict input
    "gadt"   -> case runParser parseGadt input of
                  Left e -> print e
                  Right (Wrapper a) -> print $ eval a
    _        -> error "Encountered an invalid parsing strategy."
 where
  runExpr p input = either print print (runParser p input)

main :: IO ()
main = run =<< O.execParser opts
 where
  opts = O.info (args <**> O.helper)
    ( O.fullDesc
   <> O.progDesc "Different parsing strategies using initial encoding"
   <> O.header "Initial encoding parsing"
    )
