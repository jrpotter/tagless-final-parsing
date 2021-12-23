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
import Parser.Utils (Parser)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

convert :: GExpr a -> Expr
convert (GInt  a) = EInt a
convert (GBool a) = EBool a
convert (GAdd lhs rhs) = EAdd (convert lhs) (convert rhs)
convert (GSub lhs rhs) = ESub (convert lhs) (convert rhs)
convert (GAnd lhs rhs) = EAnd (convert lhs) (convert rhs)
convert (GOr  lhs rhs) = EOr  (convert lhs) (convert rhs)

runParsers :: Text -> [Either Text Expr]
runParsers input = [runNaive, runMulPass, runMemCons, runGadt'] <*> [input]
 where
   runGadt' i = do
     Wrapper res <- runGadt i
     pure $ convert res

allEqual :: forall a. Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual [x, y] = x == y
allEqual (x:y:xs) = x == y && allEqual (y : xs)

shouldParse :: Text -> Expr -> Expectation
shouldParse input expected = do
  let res@(x : _) = runParsers input
  shouldBe x $ Right expected
  shouldBe True $ allEqual res

shouldNotParse :: Text -> Text -> Expectation
shouldNotParse input expected = do
  let res@(x : _) = runParsers input
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
