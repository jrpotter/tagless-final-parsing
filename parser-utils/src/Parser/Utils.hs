{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.Utils
( Op(..)
, Parser
, ParserT
, boolean
, integer
, lexeme
, ops
, parens
, space
, symbol
) where

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Text (Text)
import Data.Void (Void)

type Parser = M.Parsec Void Text

type ParserT = M.ParsecT Void Text

space :: forall m. ParserT m ()
space = ML.space MC.space1 M.empty M.empty
{-# INLINE space #-}

lexeme :: forall m a. ParserT m a -> ParserT m a
lexeme = ML.lexeme MC.space
{-# INLINE lexeme #-}

symbol :: forall m. Text -> ParserT m Text
symbol = ML.symbol space
{-# INLINE symbol #-}

parens :: forall m a. ParserT m a -> ParserT m a
parens = M.between (symbol "(") (symbol ")")
{-# INLINE parens #-}

boolean :: forall m. ParserT m Bool
boolean = lexeme $ MC.string "true" $> True <|> MC.string "false" $> False
{-# INLINE boolean #-}

integer :: forall m. ParserT m Integer
integer = lexeme ML.decimal
{-# INLINE integer #-}

data Op = OpAdd | OpSub | OpAnd | OpOr

instance Show Op where
  show OpAdd = "+"
  show OpSub = "-"
  show OpAnd = "&&"
  show OpOr  = "||"

ops :: forall m. ParserT m Op
ops = M.choice
  [ symbol "+"  $> OpAdd
  , symbol "-"  $> OpSub
  , symbol "&&" $> OpAnd
  , symbol "||" $> OpOr
  ]
