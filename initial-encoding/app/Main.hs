{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text.IO (hGetContents)
import Options.Applicative
import Parser.Initial
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

data Args = Args {argsFileName :: !FilePath, argsMethod :: !Text}

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

runExpr :: (Text -> Either Text Expr) -> Text -> IO ()
runExpr f input = case f input >>= eval of
  Right (EInt e) -> print e
  Right (EBool e) -> print e
  _ -> error "Could not evaluate expression fully."

run :: Args -> IO ()
run args = do
  handle <- openFile (argsFileName args) ReadMode
  contents <- hGetContents handle
  case argsMethod args of
    "naive"    -> runExpr runNaive contents
    "mul_pass" -> runExpr runMulPass contents
    "mem_cons" -> runExpr runMemCons contents
    "gadt"     -> case runGadt contents of
                    Left e -> print e
                    Right (Wrapper a) -> print $ gadtEval a
    _          -> error "Encountered an invalid parsing strategy."

main :: IO ()
main = run =<< execParser opts
 where
  opts = info (args <**> helper)
    ( fullDesc
   <> progDesc "Different parsing strategies using initial encoding"
   <> header "Initial encoding parsing"
    )
