{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Parser.FinalTest
( spec_parser,
) where

import Data.Text (Text)
import Parser.Final
import Parser.Utils
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

-- ========================================
-- Utility
-- ========================================

instance Eq (Dynamic Eval) where
  d1 == d2 = case (fromDyn @Eval @Integer d1, fromDyn @Eval @Integer d2) of
    (Just a1, Just a2) -> a1 == a2
    _ -> case (fromDyn @Eval @Bool d1, fromDyn @Eval @Bool d2) of
      (Just a1, Just a2) -> a1 == a2
      _ -> False

instance Show (Dynamic Eval) where
  show d = case fromDyn @Eval @Integer d of
    Just a -> show a
    _ -> case fromDyn @Eval @Bool d of
      Just a -> show a
      _ -> error "No valid `Eval` instance."

runParsers :: Text -> [Either Text (Dynamic Eval)]
runParsers input = [runParser parseSingle, runParser parseStrict] <*> [input]

-- ========================================
-- Assertions
-- ========================================

shouldParse :: Text -> Dynamic Eval -> Expectation
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
      shouldParse "1" (toDyn @Eval @Integer 1)
    it "true" do
      shouldParse "true" (toDyn True)
    it "false" do
      shouldParse "false" (toDyn False)
  describe "addition/subtraction" do
    it "binary" do
      shouldParse "1 + 1" (toDyn @Eval @Integer 2)
    it "left associative" do
      shouldParse "1 - 3 + 4" (toDyn @Eval @Integer 2)
    it "precedence" do
      shouldParse "1 - (3 + 4)" (toDyn @Eval @Integer (-6))
  describe "conjunction/disjunction" do
    it "binary" do
      shouldParse "true && false" (toDyn False)
      shouldParse "true && true" (toDyn True)
      shouldParse "true || true" (toDyn True)
      shouldParse "true || false" (toDyn True)
      shouldParse "false || false" (toDyn False)
  describe "invalid types" do
    it "mismatch" do
      shouldNotParse "true && 1"
        "1:9:\n  |\n1 | true && 1\n  |         ^\nInvalid operands for `&&`\n"
      shouldNotParse "1 + true"
        "1:5:\n  |\n1 | 1 + true\n  |     ^\nInvalid operands for `+`\n"
