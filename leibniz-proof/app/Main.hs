{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Eq.Type ((:=)(..), refl)

newtype F1 t b a = F1 {runF1 :: t := (a -> b)}
newtype F2 t a b = F2 {runF2 :: t := (a -> b)}

functionEquality
  :: forall a1 a2 b1 b2
   . a1 := a2
  -> b1 := b2
  -> (a1 -> b1) := (a2 -> b2)
functionEquality
  (Refl s1)  -- s1 :: forall c. c a1 -> c a2
  (Refl s2)  -- s2 :: forall c. c b1 -> c b2
  = runF2    -- (a1 -> b1) := (a2 -> b2)
  . s2       -- F2 (a1 -> b1) a2 b2
  . F2       -- F2 (a1 -> b1) a2 b1
  . runF1    -- (a1 -> b1) := (a2 -> b1)
  . s1       -- F1 (a1 -> b1) b1 a2
  . F1       -- F1 (a1 -> b1) b1 a1
  $ refl     -- (a1 -> b1) := (a1 -> b1)

main :: IO ()
main = putStrLn "Hello, Haskell!"
