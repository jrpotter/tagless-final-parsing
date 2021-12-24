{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Parser.InitialTest
( spec_parser,
) where

import qualified Text.Megaparsec as M

import Data.Bifunctor (first)
import Data.Functor.Identity (Identity(..))
import Data.Text (Text, pack)
import Parser.Initial
import Parser.Utils (Parser, allEqual, runParser)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- ========================================
-- Utility
-- ========================================

convert :: GExpr a -> Expr
convert (GInt  a) = EInt a
convert (GBool a) = EBool a
convert (GAdd lhs rhs) = EAdd (convert lhs) (convert rhs)
convert (GSub lhs rhs) = ESub (convert lhs) (convert rhs)
convert (GAnd lhs rhs) = EAnd (convert lhs) (convert rhs)
convert (GOr  lhs rhs) = EOr  (convert lhs) (convert rhs)

runParsers :: Text -> [Either Text Result]
runParsers input =
  [ runParser parseNaive
  , runParser parseSingle
  , runParser parseStrict
  , runGadt
  ] <*> [input]
 where
   runGadt i = do
     Wrapper res <- runParser parseGadt i
     toResult $ convert res

-- ========================================
-- Assertions
-- ========================================

shouldParse :: Text -> Result -> Expectation
shouldParse input expected = do
  let res@(x : _) = runParsers input
  shouldBe x $ Right expected
  shouldBe True $ allEqual res

shouldNotParse :: Text -> Text -> Expectation
shouldNotParse input expected = do
  let res@(x : _) = runParsers input
  shouldBe x $ Left expected

-- ========================================
-- Tests
-- ========================================

spec_parser :: Spec
spec_parser = do
  describe "literals" do
    it "1" do
      shouldParse "1" (RInt 1)
    it "true" do
      shouldParse "true" (RBool True)
    it "false" do
      shouldParse "false" (RBool False)
  describe "addition/subtraction" do
    it "binary" do
      shouldParse "1 + 1" (RInt 2)
    it "left associative" do
      shouldParse "1 - 3 + 4" (RInt 2)
    it "precedence" do
      shouldParse "1 - (3 + 4)" (RInt (-6))
  describe "conjunction/disjunction" do
    it "binary" do
      shouldParse "true && false" (RBool False)
      shouldParse "true && true" (RBool True)
      shouldParse "true || true" (RBool True)
      shouldParse "true || false" (RBool True)
      shouldParse "false || false" (RBool False)
  describe "invalid types" do
    it "mismatch" do
      shouldNotParse "true && 1"
        "1:10:\n  |\n1 | true && 1\n  |          ^\nCould not cast boolean.\n"
      shouldNotParse "1 + true"
        "1:9:\n  |\n1 | 1 + true\n  |         ^\nCould not cast integer.\n"
