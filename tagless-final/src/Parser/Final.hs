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
, runMemCons
, runMulPass
, toDyn
) where

import qualified Control.Monad.Combinators.Expr as E
import qualified Data.Eq.Type as EQ
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad (join)
import Control.Monad.Except (MonadError, throwError)
import Data.Bifunctor (first)
import Data.Eq.Type ((:=))
import Data.Functor (($>), void)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust)
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
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
-- Multiple passes
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

mulPassExpr :: forall repr. Symantics repr => Parser (Dynamic repr)
mulPassExpr = expr >>= \case
  Left (offset, msg) -> M.setOffset offset >> fail msg
  Right a -> pure a
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
        Nothing -> throwError
          (offset, "Invalid operands for `" <> unpack name <> "`")
        Just a -> pure a

  term = parens expr <|>
         Right . toDyn <$> integer <|>
         Right . toDyn <$> boolean

runMulPass :: forall repr. Symantics repr => Text -> Either Text (Dynamic repr)
runMulPass input =
  let res = M.runParser (mulPassExpr <* M.eof) "" input
   in first (pack . M.errorBundlePretty) res

-- ========================================
-- Memory consumption
-- ========================================

instance (NFData t) => NFData (Eval t) where
  rnf (Eval t) = t `seq` ()

instance NFData (Dynamic Eval) where
  rnf (Dynamic t e) = e `seq` ()

memConsExpr
  :: forall repr
   . Symantics repr
  => NFData (Dynamic repr)
  => Parser (Dynamic repr)
memConsExpr = term >>= expr
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

runMemCons
  :: forall repr
   . Symantics repr
  => NFData (Dynamic repr)
  => Text
  -> Either Text (Dynamic repr)
runMemCons input =
  let res = M.runParser (memConsExpr <* M.eof) "" input
   in first (pack . M.errorBundlePretty) res

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

instance (NFData t) => NFData (Print t) where
  rnf (Print t) = t `seq` ()

instance NFData (Dynamic Print) where
  rnf (Dynamic _ p) = p `seq` ()

{-
No instance for (IsDynamic Text)
Couldn't match type `Eval` with `Print`

evalStuck :: Text -> Either Text (Either Integer Bool, Text)
evalStuck input = do
  d <- runMemCons input
  let e1 = fromDyn d :: Maybe (Eval Integer)
  let e2 = fromDyn d :: Maybe (Eval Bool)
  let e3 = fromDyn d :: Maybe (Print Text)
  ...
-}

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

instance NFData (SQ t) where
  rnf _ = ()

instance NFData (Dynamic SQ) where
  rnf (Dynamic _ _) = ()

evalClosed :: Text -> Either Text (Either Integer Bool, Text)
evalClosed input = do
  d <- runMemCons input
  let e1 = fromDyn d :: Maybe (SQ Integer)
  let e2 = fromDyn d :: Maybe (SQ Bool)
  case (e1, e2) of
    (Just a, _) -> case runSQ a of
       Eval e -> case runSQ a of Print b -> pure (Left e, b)
    (_, Just a) -> case runSQ a of
       Eval e -> case runSQ a of Print b -> pure (Right e, b)
    _ -> Left "Could not cast into a integer or boolean."

-- ========================================
-- Open
-- ========================================

