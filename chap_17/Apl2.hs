module Apl1 where

import Control.Applicative 
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data List a = 
    Nil
  | Cons a (List a) 
  deriving (Eq, Show)

instance Functor List where 
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)


instance Applicative List where 
  pure a = Cons a Nil
  (<*>) fs as = flatMap (\f -> fmap f as) fs
  --(<*>) Nil _ = Nil
  --(<*>) _ Nil = Nil
  --(<*>) (Cons f fs) as = append (fmap f as) (fs <*> as)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b 
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a 
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b 
flatMap f = concat' . fmap f 
