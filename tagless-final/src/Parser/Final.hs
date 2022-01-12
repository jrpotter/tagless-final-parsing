{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.Final
  ( Dynamic (..),
    Eval (..),
    SQ (..),
    Symantics (..),
    TQ (..),
    TextSymantics (..),
    Typeable (..),
    fromDyn,
    parseSingle,
    parseStrict,
    toDyn,
    runBoth',
  )
where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad.Combinators (sepBy)
import qualified Control.Monad.Combinators.Expr as E
import Data.Eq.Type ((:=))
import qualified Data.Eq.Type as EQ
import Data.Functor (void)
import Data.Maybe (isJust)
import Data.Text (Text, drop, dropEnd, pack, unpack)
import Parser.Utils
import qualified Text.Megaparsec as M
import Prelude hiding (drop)

-- ========================================
-- Symantics
-- ========================================

class Symantics repr where
  eInt :: Integer -> repr Integer
  eBool :: Bool -> repr Bool
  eAdd :: repr Integer -> repr Integer -> repr Integer
  eSub :: repr Integer -> repr Integer -> repr Integer
  eAnd :: repr Bool -> repr Bool -> repr Bool
  eOr :: repr Bool -> repr Bool -> repr Bool

class (Symantics repr) => TextSymantics repr where
  eText :: Text -> repr Text
  eAppend :: repr Text -> repr Text -> repr Text

newtype Eval a = Eval {runEval :: a} deriving (Eq, Show)

instance Symantics Eval where
  eInt = Eval
  eBool = Eval
  eAdd (Eval lhs) (Eval rhs) = Eval (lhs + rhs)
  eSub (Eval lhs) (Eval rhs) = Eval (lhs - rhs)
  eAnd (Eval lhs) (Eval rhs) = Eval (lhs && rhs)
  eOr (Eval lhs) (Eval rhs) = Eval (lhs || rhs)

instance TextSymantics Eval where
  eText = Eval
  eAppend (Eval lhs) (Eval rhs) = Eval $ lhs <> rhs

-- ========================================
-- Typeable
-- ========================================

class Typeable repr where
  pInt :: repr Integer
  pBool :: repr Bool
  pText :: repr Text

newtype TQ t = TQ {runTQ :: forall repr. Typeable repr => repr t}

instance Typeable TQ where
  pInt = TQ pInt
  pBool = TQ pBool
  pText = TQ pText

newtype AsInt a = AsInt (Maybe (a := Integer))

instance Typeable AsInt where
  pInt = AsInt (Just EQ.refl)
  pBool = AsInt Nothing
  pText = AsInt Nothing

newtype AsBool a = AsBool (Maybe (a := Bool))

instance Typeable AsBool where
  pInt = AsBool Nothing
  pBool = AsBool (Just EQ.refl)
  pText = AsBool Nothing

newtype AsText a = AsText (Maybe (a := Text))

instance Typeable AsText where
  pInt = AsText Nothing
  pBool = AsText Nothing
  pText = AsText (Just EQ.refl)

-- ========================================
-- Dynamic
-- ========================================

data Dynamic repr = forall t. Dynamic (TQ t) (repr t)

class IsDynamic a where
  type' :: forall repr. Typeable repr => repr a
  lift' :: forall repr. TextSymantics repr => a -> repr a
  cast' :: forall repr t. TQ t -> Maybe (t := a)

instance IsDynamic Integer where
  type' = pInt
  lift' = eInt
  cast' (TQ t) = case t of AsInt a -> a

instance IsDynamic Bool where
  type' = pBool
  lift' = eBool
  cast' (TQ t) = case t of AsBool a -> a

instance IsDynamic Text where
  type' = pText
  lift' = eText
  cast' (TQ t) = case t of AsText a -> a

toDyn :: forall repr a. IsDynamic a => TextSymantics repr => a -> Dynamic repr
toDyn = Dynamic type' . lift'

fromDyn :: forall repr a. IsDynamic a => Dynamic repr -> Maybe (repr a)
fromDyn (Dynamic t e) = case t of
  (cast' -> r) -> do
    r' <- r
    pure $ EQ.coerce (EQ.lift r') e

asDyn ::
  forall repr a.
  TextSymantics repr =>
  IsDynamic a =>
  (repr a -> repr a -> repr a) ->
  Dynamic repr ->
  Dynamic repr ->
  Maybe (Dynamic repr)
asDyn bin lhs rhs = do
  lhs' <- fromDyn lhs
  rhs' <- fromDyn rhs
  pure . Dynamic type' $ bin lhs' rhs'

-- ========================================
-- Single pass
-- ========================================

parseSingle :: forall repr. TextSymantics repr => Parser (Dynamic repr)
parseSingle = expr >>= either offsetFail pure
  where
    offsetFail (offset, msg) = M.setOffset offset >> fail msg

    expr =
      E.makeExprParser
        term
        [ [binary "+" eAdd, binary "-" eSub],
          [binary "&&" eAnd, binary "||" eOr]
        ]

    binary name bin = E.InfixL do
      void $ symbol name
      offset <- M.getOffset
      pure $ \lhs rhs -> do
        lhs' <- lhs
        rhs' <- rhs
        case asDyn bin lhs' rhs' of
          Nothing -> Left (offset, "Invalid operands for `" <> unpack name <> "`")
          Just a -> pure a

    term = parens expr <|> Right . toDyn <$> integer <|> Right . toDyn <$> boolean

-- ========================================
-- Strict
-- ========================================

instance (NFData t) => NFData (Eval t) where
  rnf (Eval t) = t `seq` ()

instance NFData (Dynamic Eval) where
  rnf (Dynamic _ e) = e `seq` ()

parseStrict ::
  forall repr.
  NFData (Dynamic repr) =>
  TextSymantics repr =>
  Parser (Dynamic repr)
parseStrict = term >>= expr
  where
    expr :: Dynamic repr -> Parser (Dynamic repr)
    expr t = do
      op <- M.option Nothing $ Just <$> ops
      case op of
        Just OpAdd -> nest t eAdd OpAdd
        Just OpSub -> nest t eSub OpSub
        Just OpAnd -> nest t eAnd OpAnd
        Just OpOr -> nest t eOr OpOr
        _ -> pure t

    nest ::
      forall a.
      IsDynamic a =>
      Dynamic repr ->
      (repr a -> repr a -> repr a) ->
      Op ->
      Parser (Dynamic repr)
    nest t bin op = do
      t' <- term
      case asDyn bin t t' of
        Nothing -> fail $ "Invalid operands for `" <> show op <> "`"
        Just a -> a `deepseq` expr a

    term :: Parser (Dynamic repr)
    term = do
      p <- M.option Nothing $ Just <$> symbol "("
      if isJust p
        then (term >>= expr) <* symbol ")"
        else toDyn <$> integer <|> toDyn <$> boolean

-- ========================================
-- Pretty print
-- ========================================

newtype PPrint a = PPrint {runPPrint :: Text} deriving (Eq, Show)

instance Symantics PPrint where
  eInt = PPrint . pack . show
  eBool = PPrint . pack . show
  eAdd (PPrint lhs) (PPrint rhs) = PPrint $ "(" <> lhs <> " + " <> rhs <> ")"
  eSub (PPrint lhs) (PPrint rhs) = PPrint $ "(" <> lhs <> " - " <> rhs <> ")"
  eAnd (PPrint lhs) (PPrint rhs) = PPrint $ "(" <> lhs <> " && " <> rhs <> ")"
  eOr (PPrint lhs) (PPrint rhs) = PPrint $ "(" <> lhs <> " || " <> rhs <> ")"

instance TextSymantics PPrint where
  eText = PPrint
  eAppend (PPrint lhs) (PPrint rhs) =
    PPrint $ "(" <> lhs <> " <> " <> rhs <> ")"

-- ========================================
-- Multiplication
-- ========================================

class (Symantics repr) => MulSymantics repr where
  eMul :: repr Integer -> repr Integer -> repr Integer

instance MulSymantics Eval where
  eMul (Eval lhs) (Eval rhs) = Eval (lhs * rhs)

instance MulSymantics PPrint where
  eMul (PPrint lhs) (PPrint rhs) = PPrint $ "(" <> lhs <> " * " <> rhs <> ")"

-- ========================================
-- Closed
-- ========================================

newtype SQ a = SQ {runSQ :: forall repr. Symantics repr => repr a}

instance Symantics SQ where
  eInt e = SQ (eInt e)
  eBool e = SQ (eBool e)
  eAdd (SQ lhs) (SQ rhs) = SQ (eAdd lhs rhs)
  eSub (SQ lhs) (SQ rhs) = SQ (eSub lhs rhs)
  eAnd (SQ lhs) (SQ rhs) = SQ (eAnd lhs rhs)
  eOr (SQ lhs) (SQ rhs) = SQ (eOr lhs rhs)

newtype MSQ a = MSQ {runMSQ :: forall repr. MulSymantics repr => repr a}

instance Symantics MSQ where
  eInt e = MSQ (eInt e)
  eBool e = MSQ (eBool e)
  eAdd (MSQ lhs) (MSQ rhs) = MSQ (eAdd lhs rhs)
  eSub (MSQ lhs) (MSQ rhs) = MSQ (eSub lhs rhs)
  eAnd (MSQ lhs) (MSQ rhs) = MSQ (eAnd lhs rhs)
  eOr (MSQ lhs) (MSQ rhs) = MSQ (eOr lhs rhs)

instance MulSymantics MSQ where
  eMul (MSQ lhs) (MSQ rhs) = MSQ (eMul lhs rhs)

data Result = RInt Integer | RBool Bool

runBoth :: Dynamic SQ -> Maybe (Result, Text)
runBoth d = case fromDyn @SQ @Integer d of
  Just (SQ q) -> pure (case q of Eval a -> RInt a, case q of PPrint a -> a)
  Nothing -> case fromDyn @SQ @Bool d of
    Just (SQ q) -> pure (case q of Eval a -> RBool a, case q of PPrint a -> a)
    Nothing -> Nothing

pPrint :: Dynamic SQ -> Maybe Text
pPrint d = case fromDyn @SQ @Integer d of
  Just (SQ q) -> pure case q of PPrint a -> a
  Nothing -> case fromDyn @SQ @Bool d of
    Just (SQ q) -> pure case q of PPrint a -> a
    Nothing -> Nothing

-- Have to define a new function to work on `MulSymantics`.
pPrint' :: Dynamic MSQ -> Maybe Text
pPrint' d = case fromDyn @MSQ @Integer d of
  Just (MSQ q) -> pure case q of PPrint a -> a
  Nothing -> case fromDyn @MSQ @Bool d of
    Just (MSQ q) -> pure case q of PPrint a -> a
    Nothing -> case fromDyn @MSQ @Text d of
      Just (MSQ q) -> pure case q of PPrint a -> a
      Nothing -> Nothing

-- ========================================
-- Open
-- ========================================

data SCopy repr1 repr2 a = SCopy (repr1 a) (repr2 a)

instance
  (Symantics repr1, Symantics repr2) =>
  Symantics (SCopy repr1 repr2)
  where
  eInt e = SCopy (eInt e) (eInt e)
  eBool e = SCopy (eBool e) (eBool e)
  eAdd (SCopy a1 a2) (SCopy b1 b2) = SCopy (eAdd a1 b1) (eAdd a2 b2)
  eSub (SCopy a1 a2) (SCopy b1 b2) = SCopy (eSub a1 b1) (eSub a2 b2)
  eAnd (SCopy a1 a2) (SCopy b1 b2) = SCopy (eAnd a1 b1) (eAnd a2 b2)
  eOr (SCopy a1 a2) (SCopy b1 b2) = SCopy (eOr a1 b1) (eOr a2 b2)

instance
  (MulSymantics repr1, MulSymantics repr2) =>
  MulSymantics (SCopy repr1 repr2)
  where
  eMul (SCopy a1 a2) (SCopy b1 b2) = SCopy (eMul a1 b1) (eMul a2 b2)

runEval' ::
  forall repr.
  Dynamic (SCopy Eval repr) ->
  Maybe (Result, Dynamic repr)
runEval' d = case fromDyn d :: Maybe (SCopy Eval repr Integer) of
  Just (SCopy (Eval a) r) -> pure (RInt a, Dynamic pInt r)
  Nothing -> case fromDyn d :: Maybe (SCopy Eval repr Bool) of
    Just (SCopy (Eval a) r) -> pure (RBool a, Dynamic pBool r)
    Nothing -> Nothing

runPPrint' ::
  forall repr.
  Dynamic (SCopy PPrint repr) ->
  Maybe (Text, Dynamic repr)
runPPrint' d = case fromDyn d :: Maybe (SCopy PPrint repr Text) of
  Just (SCopy (PPrint a) r) -> pure (a, Dynamic pText r)
  Nothing -> Nothing

runBoth' ::
  forall repr.
  Dynamic (SCopy Eval (SCopy PPrint repr)) ->
  Maybe (Result, Text, Dynamic repr)
runBoth' d = do
  (r, d') <- runEval' d
  (p, d'') <- runPPrint' d'
  pure (r, p, d'')
