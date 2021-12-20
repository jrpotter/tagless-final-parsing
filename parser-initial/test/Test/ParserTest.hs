{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ParserTest
( spec_parser,
) where

import qualified Text.Megaparsec as M

import Data.Text (Text, pack)
import Parser
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

runParser :: Parser Expr -> Text -> IO (Either Text Expr)
runParser m input = pure case M.parse (m <* M.eof) "ParserTest" input of
  Left e -> Left . pack $ M.errorBundlePretty e
  Right a -> eval a

runParsers :: Text -> IO [Either Text Expr]
runParsers input =
  mapM (`runParser` input) [naiveExpr, mulPassExpr, memConsExpr]

allEqual :: forall a. Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual [x, y] = x == y
allEqual (x:y:xs) = x == y && allEqual (y : xs)

shouldParse :: Text -> Expr -> Expectation
shouldParse input expected = do
  res@(x : _) <- runParsers input
  shouldBe x $ Right expected
  shouldBe True $ allEqual res

shouldNotParse :: Text -> Text -> Expectation
shouldNotParse input expected = do
  res@(x : _) <- runParsers input
  shouldBe x $ Left expected

spec_parser :: Spec
spec_parser = do
  describe "literals" do
    it "1" do
      shouldParse "1" (EInt 1)
    it "true" do
      shouldParse "true" (EBool True)
    it "false" do
      shouldParse "false" (EBool False)
  describe "addition/subtraction" do
    it "binary" do
      shouldParse "1 + 1" (EInt 2)
    it "left associative" do
      shouldParse "1 - 3 + 4" (EInt 2)
    it "precedence" do
      shouldParse "1 - (3 + 4)" (EInt (-6))
  describe "conjunction/disjunction" do
    it "binary" do
      shouldParse "true && false" (EBool False)
      shouldParse "true && true" (EBool True)
      shouldParse "true || true" (EBool True)
      shouldParse "true || false" (EBool True)
      shouldParse "false || false" (EBool False)
  describe "invalid types" do
    it "mismatch" do
      shouldNotParse "true && 1" "Expected two booleans."
      shouldNotParse "1 + true" "Expected two integers."
