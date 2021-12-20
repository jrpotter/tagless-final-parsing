{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Control.Monad.Combinators.Expr as E
import qualified Data.Eq.Type as EQ
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.Eq.Type ((:=))
import Data.Foldable (foldl')
import Data.Functor (($>), void)
import Data.Proxy (Proxy(..))
import Data.Text (Text, unpack)
import Data.Text.IO (hGetContents)
import Data.Void (Void)
import Numeric (readDec)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile)

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

newtype SQ a = SQ {runSQ :: forall repr. Symantics repr => repr a}

instance Symantics SQ where
  eInt  e = SQ (eInt e)
  eBool e = SQ (eBool e)
  eAdd (SQ lhs) (SQ rhs) = SQ (eAdd lhs rhs)
  eSub (SQ lhs) (SQ rhs) = SQ (eSub lhs rhs)
  eAnd (SQ lhs) (SQ rhs) = SQ (eAnd lhs rhs)
  eOr  (SQ lhs) (SQ rhs) = SQ (eOr  lhs rhs)

newtype Expr a = Expr {runExpr :: a} deriving Show

instance Symantics Expr where
  eInt  = Expr
  eBool = Expr
  eAdd (Expr lhs) (Expr rhs) = Expr (lhs + rhs)
  eSub (Expr lhs) (Expr rhs) = Expr (lhs - rhs)
  eAnd (Expr lhs) (Expr rhs) = Expr (lhs && rhs)
  eOr  (Expr lhs) (Expr rhs) = Expr (lhs || rhs)

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
-- Parser code
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

mkBinary
  :: forall repr a
   . Symantics repr
  => IsDynamic a
  => (repr a -> repr a -> repr a)
  -> Dynamic repr
  -> Dynamic repr
  -> Maybe (Dynamic repr)
mkBinary bin lhs rhs = do
  lhs' <- fromDyn lhs
  rhs' <- fromDyn rhs
  pure . Dynamic type' $ bin lhs' rhs'

expr :: forall repr. Symantics repr => Parser (Dynamic repr)
expr = expr' >>= \case
  Left (offset, msg) -> M.setOffset offset >> fail msg
  Right a -> pure a
 where
  expr' = E.makeExprParser
    (parens expr' <|> Right . toDyn <$> integer <|> Right . toDyn <$> boolean)
    [ [binary' "+" eAdd, binary' "-" eSub]
    , [binary' "&&" eAnd, binary' "||" eOr]
    ]

  binary' name bin = E.InfixL do
    void $ symbol name
    offset <- M.getOffset
    pure $ \lhs rhs -> do
      lhs' <- lhs
      rhs' <- rhs
      case mkBinary bin lhs' rhs' of
        Nothing -> Left (offset, "Invalid operands for `" <> unpack name <> "`")
        Just a -> pure a

-- ========================================
-- Main
-- ========================================

main :: IO ()
main = do
  [fileName] <- getArgs
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  case M.parse expr fileName contents of
    Left e -> print $ M.errorBundlePretty e
    Right a -> print (fromDyn a :: Maybe (Expr Integer))
