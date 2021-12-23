{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Initial
( Expr(..)
, GExpr(..)
, Wrapper(..)
, eval
, gadtEval
, runGadt
, runMemCons
, runMulPass
, runNaive
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

eval :: Expr -> Either Text Expr
eval e@(EInt  _) = pure e
eval e@(EBool _) = pure e
eval (EAdd lhs rhs) = do
  (lhs', rhs') <- binInt lhs rhs
  pure $ EInt (lhs' + rhs')
eval (ESub lhs rhs) = do
  (lhs', rhs') <- binInt lhs rhs
  pure $ EInt (lhs' - rhs')
eval (EAnd lhs rhs) = do
  (lhs', rhs') <- binBool lhs rhs
  pure $ EBool (lhs' && rhs')
eval (EOr lhs rhs) = do
  (lhs', rhs') <- binBool lhs rhs
  pure $ EBool (lhs' || rhs')

binInt :: Expr -> Expr -> Either Text (Integer, Integer)
binInt lhs rhs = do
  lhs' <- eval lhs
  rhs' <- eval rhs
  case (lhs', rhs') of
    (EInt lhs'', EInt rhs'') -> pure (lhs'', rhs'')
    _ -> Left "Expected two integers."

binBool :: Expr -> Expr -> Either Text (Bool, Bool)
binBool lhs rhs = do
  lhs' <- eval lhs
  rhs' <- eval rhs
  case (lhs', rhs') of
    (EBool lhs'', EBool rhs'') -> pure (lhs'', rhs'')
    _ -> Left "Expected two booleans."

-- ========================================
-- Naive attempt
-- ========================================

naiveExpr :: forall m. ParserT m Expr
naiveExpr = E.makeExprParser term
  [ [binary "+" EAdd, binary "-" ESub]
  , [binary "&&" EAnd, binary "||" EOr]
  ]
 where
  binary name f = E.InfixL (f <$ symbol name)

  term = parens naiveExpr <|>
         EInt <$> integer <|>
         EBool <$> boolean

runNaive :: Text -> Either Text Expr
runNaive input =
  let res = M.parse (naiveExpr <* M.eof) "" input
   in join $ bimap (pack . M.errorBundlePretty) eval res

-- ========================================
-- Multiple passes
-- ========================================

mulPassExpr :: forall m. MonadError Text m => ParserT m Expr
mulPassExpr = expr >>= either (fail . unpack) pure
 where
  expr = E.makeExprParser term
    [ [ binary "+"  binInt  EInt  EAdd
      , binary "-"  binInt  EInt  ESub
      ]
    , [ binary "&&" binBool EBool EAnd
      , binary "||" binBool EBool EOr
      ]
    ]

  binary name cast f bin = E.InfixL do
    void $ symbol name
    pure $ \lhs rhs -> do
      lhs' <- lhs
      rhs' <- rhs
      (lhs', rhs') <- cast lhs' rhs'
      eval $ bin (f lhs') (f rhs')

  term = parens expr <|>
         Right . EInt <$> integer <|>
         Right . EBool <$> boolean

runMulPass :: Text -> Either Text Expr
runMulPass input =
  let res = M.runParserT (mulPassExpr <* M.eof) "" input
   in res >>= join . bimap (pack . M.errorBundlePretty) eval

-- ========================================
-- Memory consumption
-- ========================================

instance NFData Expr where
  rnf (EInt  e) = rnf e
  rnf (EBool e) = rnf e
  rnf (EAdd lhs rhs) = rnf lhs `seq` rnf rhs
  rnf (ESub lhs rhs) = rnf lhs `seq` rnf rhs
  rnf (EAnd lhs rhs) = rnf lhs `seq` rnf rhs
  rnf (EOr  lhs rhs) = rnf lhs `seq` rnf rhs

memConsExpr :: forall m. MonadError Text m => ParserT m Expr
memConsExpr = term >>= expr
 where
  expr t = do
    op <- M.option Nothing $ Just <$> ops
    case op of
      Just OpAdd -> nest t EAdd
      Just OpSub -> nest t ESub
      Just OpAnd -> nest t EAnd
      Just OpOr  -> nest t EOr
      _          -> pure t

  nest :: Expr -> (Expr -> Expr -> Expr) -> ParserT m Expr
  nest t bin = do
    t' <- term
    case eval (bin t t') of
      Left e -> throwError e
      Right a -> a `deepseq` expr a

  term = do
    p <- M.option Nothing $ Just <$> symbol "("
    if isJust p then (term >>= expr) <* symbol ")" else
      EInt <$> integer <|> EBool <$> boolean

runMemCons :: Text -> Either Text Expr
runMemCons input =
  let res = M.runParserT (memConsExpr <* M.eof) "" input
   in res >>= join . bimap (pack . M.errorBundlePretty) eval

-- ========================================
-- GADTs
-- ========================================

data GExpr a where
  GInt  :: Integer -> GExpr Integer
  GBool :: Bool    -> GExpr Bool
  GAdd  :: GExpr Integer -> GExpr Integer -> GExpr Integer
  GSub  :: GExpr Integer -> GExpr Integer -> GExpr Integer
  GAnd  :: GExpr Bool    -> GExpr Bool    -> GExpr Bool
  GOr   :: GExpr Bool    -> GExpr Bool    -> GExpr Bool

instance NFData (GExpr a) where
  rnf (GInt  e) = rnf e
  rnf (GBool e) = rnf e
  rnf (GAdd lhs rhs) = rnf lhs `seq` rnf rhs
  rnf (GSub lhs rhs) = rnf lhs `seq` rnf rhs
  rnf (GAnd lhs rhs) = rnf lhs `seq` rnf rhs
  rnf (GOr  lhs rhs) = rnf lhs `seq` rnf rhs

data Wrapper = forall a. Show a => Wrapper (GExpr a)

fromInt :: GExpr a -> Either Text (GExpr Integer)
fromInt a@(GInt _  ) = pure a
fromInt a@(GAdd _ _) = pure a
fromInt a@(GSub _ _) = pure a
fromInt _            = Left "Expected an integer type."

fromBool :: GExpr a -> Either Text (GExpr Bool)
fromBool a@(GBool _  ) = pure a
fromBool a@(GAnd  _ _) = pure a
fromBool a@(GOr   _ _) = pure a
fromBool _             = Left "Expected a boolean type."

gadtEval :: GExpr a -> a
gadtEval (GInt a)  = a
gadtEval (GBool a) = a
gadtEval (GAdd lhs rhs) = gadtEval lhs + gadtEval rhs
gadtEval (GSub lhs rhs) = gadtEval lhs - gadtEval rhs
gadtEval (GAnd lhs rhs) = gadtEval lhs && gadtEval rhs
gadtEval (GOr lhs rhs)  = gadtEval lhs || gadtEval rhs

gadtExpr :: forall m. MonadError Text m => ParserT m Wrapper
gadtExpr = term >>= expr
 where
  expr t = do
    op <- M.option Nothing $ Just <$> ops
    case op of
      Just OpAdd -> nest t fromInt  GAdd GInt
      Just OpSub -> nest t fromInt  GSub GInt
      Just OpAnd -> nest t fromBool GAnd GBool
      Just OpOr  -> nest t fromBool GOr  GBool
      _          -> pure t

  nest
    :: forall b
     . Show b
    => Wrapper
    -> (forall a. GExpr a -> Either Text (GExpr b))
    -> (GExpr b -> GExpr b -> GExpr b)
    -> (b -> GExpr b)
    -> ParserT m Wrapper
  nest (Wrapper t) cast bin f = do
    Wrapper t' <- term
    case (cast t, cast t') of
      (Right lhs, Right rhs) -> do
        let z = f . gadtEval $ bin lhs rhs
        z `deepseq` expr (Wrapper z)
      (Left e, _) -> throwError e
      (_, Left e) -> throwError e

  term = do
    p <- M.option Nothing $ Just <$> symbol "("
    if isJust p then (term >>= expr) <* symbol ")" else
      Wrapper . GInt <$> integer <|> Wrapper . GBool <$> boolean

runGadt :: Text -> Either Text Wrapper
runGadt input =
  let res = M.runParserT (gadtExpr <* M.eof) "" input
   in res >>= first (pack . M.errorBundlePretty)
