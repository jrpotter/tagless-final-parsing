{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import qualified Control.Monad.Combinators.Expr as E
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)
import Numeric (readDec)
import System.Environment (getArgs)

-- ========================================
-- GADT
-- ========================================

data Expr a where
  EInt  :: Integer -> Expr Integer
  EBool :: Bool -> Expr Bool
  EAdd  :: Expr Integer -> Expr Integer -> Expr Integer
  ESub  :: Expr Integer -> Expr Integer -> Expr Integer
  EAnd  :: Expr Bool -> Expr Bool -> Expr Bool
  EOr   :: Expr Bool -> Expr Bool -> Expr Bool

deriving instance Show (Expr Integer)
deriving instance Show (Expr Bool)

eval :: forall a. Expr a -> Either Bool Integer
eval (EInt e) = Right e
eval (EBool e) = Left e
eval (EAdd lhs rhs) =
  let Right r1 = eval lhs
      Right r2 = eval rhs
   in Right (r1 + r2)
eval (ESub lhs rhs) =
  let Right r1 = eval lhs
      Right r2 = eval rhs
   in Right (r1 - r2)
eval (EAnd lhs rhs) =
  let Left r1 = eval lhs
      Left r2 = eval rhs
   in Left (r1 && r2)
eval (EOr lhs rhs) =
  let Left r1 = eval lhs
      Left r2 = eval rhs
   in Left (r1 || r2)

-- ========================================
-- Unused parser code
-- ========================================

type Parser = M.Parsec Void Text

space :: Parser ()
space = ML.space MC.space1 M.empty M.empty
{-# INLINE space #-}

lexeme_ :: forall a. Parser a -> Parser a
lexeme_ = ML.lexeme $ MC.space1 <|> M.eof
{-# INLINE lexeme_ #-}

symbol :: Text -> Parser Text
symbol = ML.symbol space
{-# INLINE symbol #-}

parens :: forall a. Parser a -> Parser a
parens = M.between (symbol "(") (symbol ")")
{-# INLINE parens #-}

boolean :: Parser Bool
boolean = lexeme_ do
  MC.string "true" $> True <|> MC.string "false" $> False
{-# INLINE boolean #-}

integer :: Parser Integer
integer = lexeme_ do
  i <- M.some $ M.satisfy isDigit
  case readDec i of
    [(value, "")] -> pure value
    _ -> fail "integer"
{-# INLINE integer #-}

{-
Couldn't match type `Bool` with `Integer`

parseExpr = E.makeExprParser parseTerm
  [ [binary "+" EAdd, binary "-" ESub]
  , [binary "&&" EAnd, binary "||" EOr]
  ]
 where
  binary name f = E.InfixL  (f <$ symbol name)
  parseTerm = parens parseExpr <|>
              EInt <$> integer <|>
              EBool <$> boolean
-}

-- ========================================
-- Main
-- ========================================

main :: IO ()
main = do
  [count] <- map read <$> getArgs
  let expr = foldl' EAdd (EInt 0) $ take count (EInt <$> [1..])
  print $ {-# SCC "evaluated" #-} eval expr
