{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Parser.Final
( Dynamic(..)
, Eval(..)
, SQ(..)
, Symantics(..)
, TQ(..)
, Typeable(..)
, fromDyn
, parseSingle
, parseStrict
, toDyn
) where

import qualified Control.Monad.Combinators.Expr as E
import qualified Data.Eq.Type as EQ
import qualified Text.Megaparsec as M

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..), deepseq)
import Data.Eq.Type ((:=))
import Data.Functor (void)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Parser.Utils

-- ========================================
-- Symantics
-- ========================================

class Symantics repr where
  eInt  :: Integer -> repr Integer
  eBool :: Bool -> repr Bool
  eAdd  :: repr Integer -> repr Integer -> repr Integer
  eSub  :: repr Integer -> repr Integer -> repr Integer
  eAnd  :: repr Bool -> repr Bool -> repr Bool
  eOr   :: repr Bool -> repr Bool -> repr Bool

newtype Eval a = Eval {runEval :: a} deriving (Eq, Show)

instance Symantics Eval where
  eInt  = Eval
  eBool = Eval
  eAdd (Eval lhs) (Eval rhs) = Eval (lhs + rhs)
  eSub (Eval lhs) (Eval rhs) = Eval (lhs - rhs)
  eAnd (Eval lhs) (Eval rhs) = Eval (lhs && rhs)
  eOr  (Eval lhs) (Eval rhs) = Eval (lhs || rhs)

-- ========================================
-- Typeable
-- ========================================

class Typeable repr where
  pInt  :: repr Integer
  pBool :: repr Bool

newtype TQ t = TQ {runTQ :: forall repr. Typeable repr => repr t}

instance Typeable TQ where
  pInt  = TQ pInt
  pBool = TQ pBool

newtype AsInt a = AsInt (Maybe (a := Integer))

instance Typeable AsInt where
  pInt  = AsInt (Just EQ.refl)
  pBool = AsInt Nothing

newtype AsBool a = AsBool (Maybe (a := Bool))

instance Typeable AsBool where
  pInt  = AsBool Nothing
  pBool = AsBool (Just EQ.refl)

-- ========================================
-- Dynamic
-- ========================================

data Dynamic repr = forall t. Dynamic (TQ t) (repr t)

class IsDynamic a where
  type' :: forall repr. Typeable repr => repr a
  lift' :: forall repr. Symantics repr => a -> repr a
  cast' :: forall repr t. TQ t -> Maybe (t := a)

instance IsDynamic Integer where
  type' = pInt
  lift' = eInt
  cast' (TQ t) = case t of AsInt a -> a

instance IsDynamic Bool where
  type' = pBool
  lift' = eBool
  cast' (TQ t) = case t of AsBool a -> a

toDyn :: forall repr a. IsDynamic a => Symantics repr => a -> Dynamic repr
toDyn = Dynamic type' . lift'

fromDyn :: forall repr a. IsDynamic a => Dynamic repr -> Maybe (repr a)
fromDyn (Dynamic t e) = case t of
  (cast' -> r) -> do
    r' <- r
    pure $ EQ.coerce (EQ.lift r') e

-- ========================================
-- Single pass
-- ========================================

binDyn
  :: forall repr a
   . Symantics repr
  => IsDynamic a
  => (repr a -> repr a -> repr a)
  -> Dynamic repr
  -> Dynamic repr
  -> Maybe (Dynamic repr)
binDyn bin lhs rhs = do
  lhs' <- fromDyn lhs
  rhs' <- fromDyn rhs
  pure . Dynamic type' $ bin lhs' rhs'

parseSingle :: forall repr. Symantics repr => Parser (Dynamic repr)
parseSingle =
  let ferr (offset, msg) = M.setOffset offset >> fail msg
   in expr >>= either ferr pure
 where
  expr = E.makeExprParser term
    [ [binary "+" eAdd, binary "-" eSub]
    , [binary "&&" eAnd, binary "||" eOr]
    ]

  binary name bin = E.InfixL do
    void $ symbol name
    offset <- M.getOffset
    pure $ \lhs rhs -> do
      lhs' <- lhs
      rhs' <- rhs
      case binDyn bin lhs' rhs' of
        Nothing -> Left (offset, "Invalid operands for `" <> unpack name <> "`")
        Just a -> pure a

  term = parens expr <|>
         Right . toDyn <$> integer <|>
         Right . toDyn <$> boolean

-- ========================================
-- Strict
-- ========================================

instance (NFData t) => NFData (Eval t) where
  rnf (Eval t) = t `seq` ()

instance NFData (Dynamic Eval) where
  rnf (Dynamic t e) = e `seq` ()

parseStrict
  :: forall repr
   . Symantics repr
  => NFData (Dynamic repr)
  => Parser (Dynamic repr)
parseStrict = term >>= expr
 where
  expr :: Dynamic repr -> Parser (Dynamic repr)
  expr t = do
    op <- M.option Nothing $ Just <$> ops
    case op of
      Just OpAdd -> nest t eAdd OpAdd
      Just OpSub -> nest t eSub OpSub
      Just OpAnd -> nest t eAnd OpAnd
      Just OpOr  -> nest t eOr  OpOr
      _          -> pure t

  nest
    :: forall a
     . IsDynamic a
    => Dynamic repr
    -> (repr a -> repr a -> repr a)
    -> Op
    -> Parser (Dynamic repr)
  nest t bin op = do
    t' <- term
    case binDyn bin t t' of
      Nothing -> fail $ "Invalid operands for `" <> show op <> "`"
      Just a -> a `deepseq` expr a

  term :: Parser (Dynamic repr)
  term = do
    p <- M.option Nothing $ Just <$> symbol "("
    if isJust p then (term >>= expr) <* symbol ")" else
      toDyn <$> integer <|> toDyn <$> boolean

-- ========================================
-- Printer
-- ========================================

newtype Print a = Print {runPrint :: Text} deriving (Eq, Show)

instance Symantics Print where
  eInt  = Print . pack . show
  eBool = Print . pack . show
  eAdd (Print lhs) (Print rhs) = Print ("(" <> lhs <> " + " <> rhs <> ")")
  eSub (Print lhs) (Print rhs) = Print ("(" <> lhs <> " - " <> rhs <> ")")
  eAnd (Print lhs) (Print rhs) = Print ("(" <> lhs <> " && " <> rhs <> ")")
  eOr  (Print lhs) (Print rhs) = Print ("(" <> lhs <> " || " <> rhs <> ")")

-- ========================================
-- Closed
-- ========================================

newtype SQ a = SQ {runSQ :: forall repr. Symantics repr => repr a}

instance Symantics SQ where
  eInt  e = SQ (eInt e)
  eBool e = SQ (eBool e)
  eAdd (SQ lhs) (SQ rhs) = SQ (eAdd lhs rhs)
  eSub (SQ lhs) (SQ rhs) = SQ (eSub lhs rhs)
  eAnd (SQ lhs) (SQ rhs) = SQ (eAnd lhs rhs)
  eOr  (SQ lhs) (SQ rhs) = SQ (eOr  lhs rhs)
