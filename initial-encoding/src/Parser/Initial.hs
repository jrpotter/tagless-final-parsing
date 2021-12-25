{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Initial
( Expr(..)
, GExpr(..)
, Result(..)
, Wrapper(..)
, eval
, parseGadt
, parseNaive
, parseSingle
, parseStrict
, toResult
) where

import qualified Control.Monad.Combinators.Expr as E
import qualified Text.Megaparsec as M

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad (join)
import Control.Monad.Except (MonadError, throwError)
import Data.Bifunctor (bimap, first)
import Data.Functor (void)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Parser.Utils

-- ========================================
-- ADT
-- ========================================

data Expr
  = EInt  Integer
  | EBool Bool
  | EAdd  Expr Expr
  | ESub  Expr Expr
  | EAnd  Expr Expr
  | EOr   Expr Expr
  deriving (Eq, Show)

data Result = RInt Integer | RBool Bool deriving (Eq)

instance Show Result where
  show (RInt e)  = show e
  show (RBool e) = show e

asInt :: Result -> Either Text Integer
asInt (RInt e) = pure e
asInt _ = Left "Could not cast integer."

asBool :: Result -> Either Text Bool
asBool (RBool e) = pure e
asBool _ = Left "Could not cast boolean."

toResult :: Expr -> Either Text Result
toResult (EInt e)  = pure $ RInt e
toResult (EBool e) = pure $ RBool e
toResult (EAdd lhs rhs) = do
  lhs' <- toResult lhs >>= asInt
  rhs' <- toResult rhs >>= asInt
  pure $ RInt (lhs' + rhs')
toResult (ESub lhs rhs) = do
  lhs' <- toResult lhs >>= asInt
  rhs' <- toResult rhs >>= asInt
  pure $ RInt (lhs' - rhs')
toResult (EAnd lhs rhs) = do
  lhs' <- toResult lhs >>= asBool
  rhs' <- toResult rhs >>= asBool
  pure $ RBool (lhs' && rhs')
toResult (EOr lhs rhs) = do
  lhs' <- toResult lhs >>= asBool
  rhs' <- toResult rhs >>= asBool
  pure $ RBool (lhs' || rhs')

-- ========================================
-- Naive attempt
-- ========================================

parseNaive :: Parser Result
parseNaive = expr >>= either (fail . unpack) pure . toResult
 where
  expr = E.makeExprParser term
    [ [binary "+" EAdd, binary "-" ESub]
    , [binary "&&" EAnd, binary "||" EOr]
    ]

  binary name f = E.InfixL (f <$ symbol name)

  term = parens expr <|> EInt <$> integer <|> EBool <$> boolean

-- ========================================
-- Single pass
-- ========================================

parseSingle :: Parser Result
parseSingle = expr >>= either (fail . unpack) pure
 where
  expr = E.makeExprParser term
    [ [binary "+"  asInt  EInt  EAdd, binary "-"  asInt  EInt  ESub]
    , [binary "&&" asBool EBool EAnd, binary "||" asBool EBool EOr ]
    ]

  binary name cast f bin = E.InfixL do
    void $ symbol name
    pure $ \lhs rhs -> do
      lhs' <- lhs >>= cast
      rhs' <- rhs >>= cast
      toResult $ bin (f lhs') (f rhs')

  term = parens expr <|> Right . RInt <$> integer <|> Right . RBool <$> boolean

-- ========================================
-- Strict
-- ========================================

instance NFData Result where
  rnf (RInt e) = e `seq` ()
  rnf (RBool e) = e `seq` ()

parseStrict :: Parser Result
parseStrict = term >>= expr
 where
  expr t = do
    op <- M.option Nothing $ Just <$> ops
    case op of
      Just OpAdd -> nest t asInt  EInt  EAdd
      Just OpSub -> nest t asInt  EInt  ESub
      Just OpAnd -> nest t asBool EBool EAnd
      Just OpOr  -> nest t asBool EBool EOr
      _          -> pure t

  nest
    :: forall a
     . Result
    -> (Result -> Either Text a)
    -> (a -> Expr)
    -> (Expr -> Expr -> Expr)
    -> Parser Result
  nest t cast f bin = do
    t' <- term
    a <- either (fail . unpack) pure do
      lhs <- cast t
      rhs <- cast t'
      toResult $ bin (f lhs) (f rhs)
    a `deepseq` expr a

  term = do
    p <- M.option Nothing $ Just <$> symbol "("
    if isJust p then (term >>= expr) <* symbol ")" else
      RInt <$> integer <|> RBool <$> boolean

-- ========================================
-- GADTs
-- ========================================

data GExpr a where
  GInt  :: Integer -> GExpr Integer
  GBool :: Bool -> GExpr Bool
  GAdd  :: GExpr Integer -> GExpr Integer -> GExpr Integer
  GSub  :: GExpr Integer -> GExpr Integer -> GExpr Integer
  GAnd  :: GExpr Bool -> GExpr Bool -> GExpr Bool
  GOr   :: GExpr Bool -> GExpr Bool -> GExpr Bool

data Wrapper = forall a. Show a => Wrapper (GExpr a)

eval :: GExpr a -> a
eval (GInt a)  = a
eval (GBool a) = a
eval (GAdd lhs rhs) = eval lhs + eval rhs
eval (GSub lhs rhs) = eval lhs - eval rhs
eval (GAnd lhs rhs) = eval lhs && eval rhs
eval (GOr lhs rhs)  = eval lhs || eval rhs

asInt' :: GExpr a -> Either Text (GExpr Integer)
asInt' a@(GInt _  ) = pure a
asInt' a@(GAdd _ _) = pure a
asInt' a@(GSub _ _) = pure a
asInt' _            = Left "Expected an integer type."

asBool' :: GExpr a -> Either Text (GExpr Bool)
asBool' a@(GBool _  ) = pure a
asBool' a@(GAnd  _ _) = pure a
asBool' a@(GOr   _ _) = pure a
asBool' _             = Left "Expected a boolean type."

parseGadt :: Parser Wrapper
parseGadt = term >>= expr
 where
  expr t = do
    op <- M.option Nothing $ Just <$> ops
    case op of
      Just OpAdd -> nest t asInt'  GInt  GAdd
      Just OpSub -> nest t asInt'  GInt  GSub
      Just OpAnd -> nest t asBool' GBool GAnd
      Just OpOr  -> nest t asBool' GBool GOr
      _          -> pure t

  nest
    :: forall b
     . Show b
    => Wrapper
    -> (forall a. GExpr a -> Either Text (GExpr b))
    -> (b -> GExpr b)
    -> (GExpr b -> GExpr b -> GExpr b)
    -> Parser Wrapper
  nest (Wrapper t) cast f bin = do
    Wrapper t' <- term
    case (cast t, cast t') of
      (Right lhs, Right rhs) -> do
        let z = eval $ bin lhs rhs
        z `seq` expr (Wrapper $ f z)
      (Left e, _) -> fail $ unpack e
      (_, Left e) -> fail $ unpack e

  term = do
    p <- M.option Nothing $ Just <$> symbol "("
    if isJust p then (term >>= expr) <* symbol ")" else
      Wrapper . GInt <$> integer <|> Wrapper . GBool <$> boolean
