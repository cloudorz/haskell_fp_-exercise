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

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), 
                         (5, Cons <$> arbitrary <*> arbitrary)]

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

take' :: Int -> List a -> List a 
take' _ Nil = Nil
take' 0 _ = Nil
take' c (Cons a as) = Cons a (take' (c - 1) as)

take'' :: Int -> ZipList' a -> ZipList' a 
take'' c (ZipList' as) = ZipList' (take' c as)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where 
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs 
                in take' 3000 l
          ys' = let (ZipList' l) = ys in
                take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where 
  pure = ZipList' . toMyList . repeat
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  --(<*>) (ZipList' fs) (ZipList' as) = ZipList' (fs <*> as)
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons a as)) = let h = f a
                                                            ZipList' xs = ZipList' fs <*> ZipList' as
                                                        in ZipList' (Cons h xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

toMyList :: [a] -> List a
toMyList = foldr Cons Nil
