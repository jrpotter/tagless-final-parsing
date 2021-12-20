{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
( eval
, memConsExpr
, mulPassExpr
, naiveExpr
) where

import qualified Control.Monad.Combinators.Expr as E
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad.State (MonadState, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Functor (($>), void)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Data.Text.IO (hGetContents)
import Data.Void (Void)
import Numeric (readDec)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

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
  deriving (Show)

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
-- Lexers
-- ========================================

type Parser = M.Parsec Void Text

space :: Parser ()
space = ML.space MC.space1 M.empty M.empty
{-# INLINE space #-}

lexeme :: forall a. Parser a -> Parser a
lexeme = ML.lexeme MC.space
{-# INLINE lexeme #-}

symbol :: Text -> Parser Text
symbol = ML.symbol space
{-# INLINE symbol #-}

parens :: forall a. Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")
{-# INLINE parens #-}

boolean :: Parser Bool
boolean = lexeme $ MC.string "true" $> True <|> MC.string "false" $> False
{-# INLINE boolean #-}

integer :: Parser Integer
integer = lexeme ML.decimal
{-# INLINE integer #-}

-- ========================================
-- Naive attempt
-- ========================================

naiveExpr :: Parser Expr
naiveExpr = E.makeExprParser term
  [ [binary "+" EAdd, binary "-" ESub]
  , [binary "&&" EAnd, binary "||" EOr]
  ]
 where
  binary name f = E.InfixL (f <$ symbol name)

  term = parens naiveExpr <|>
         EInt <$> integer <|>
         EBool <$> boolean

-- ========================================
-- Multiple passes
-- ========================================

mulPassExpr :: Parser Expr
mulPassExpr = expr >>= either (fail . unpack) pure
 where
  expr = E.makeExprParser term
    [ [binary "+"  binInt  EInt  EAdd, binary "-"  binInt  EInt  ESub]
    , [binary "&&" binBool EBool EAnd, binary "||" binBool EBool EOr]
    ]

  binary name b f op = E.InfixL do
    void $ symbol name
    pure $ \lhs rhs -> do
      lhs' <- lhs
      rhs' <- rhs
      (lhs', rhs') <- b lhs' rhs'
      eval $ op (f lhs') (f rhs')

  term = parens expr <|>
         Right . EInt <$> integer <|>
         Right . EBool <$> boolean

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

memConsExpr :: Parser Expr
memConsExpr = do
  e <- runExceptT $ term >>= expr
  either (fail . unpack) pure e
 where
  expr :: Expr -> ExceptT Text Parser Expr
  expr t = do
    op <- lift $ M.option Nothing $ Just <$> M.choice
      [symbol "+", symbol "-", symbol "&&", symbol "||"]
    case op of
      Just "+"  -> nest t EAdd
      Just "-"  -> nest t ESub
      Just "&&" -> nest t EAnd
      Just "||" -> nest t EOr
      _         -> pure t

  nest t f = do
    t' <- term
    z <- hoistEither . eval $ f t t'
    -- Need to reduce to NF for strictness guarantees.
    z `deepseq` expr z

  term = do
    p <- lift $ M.option Nothing $ Just <$> symbol "("
    if isJust p then (term >>= expr) <* lift (symbol ")") else
      lift $ EInt <$> integer <|> EBool <$> boolean
