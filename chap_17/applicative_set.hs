module Apl4 where

import Control.Applicative 
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers 
import Test.QuickCheck.Classes

data Validation e a = 
    Failure' e
  | Success' a 
  deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where 
  fmap f (Success' a) = Success' $ f a
  fmap _ (Failure' e) = Failure' e

-- This is different
instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' e1) (Failure' e2) = Failure' $ e1 <> e2
  (<*>) (Failure' e) _ = Failure' e
  (<*>) _ (Failure' e) = Failure' e
  (<*>) (Success' f) (Success' a) = Success' (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = frequency [(1, Failure' <$> arbitrary),
                         (3, Success' <$> arbitrary)]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (Failure' e1) =-= (Failure' e2) = e1 `eq` e2
  (Failure' e1) =-= _ = property False
  _ =-= (Failure' e2) = property False
  (Success' a) =-= (Success' b) = a `eq` b
                          
trigger = undefined::Validation String (String, [String], Int)

data Pair a = Pair a a deriving ( Eq, Show )

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a b) = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

pairTrigger = undefined::Pair (String, [String], Int)

data Two a b = Two a b deriving ( Eq, Show )

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a f) <*> (Two b v) = Two (a <> b) (f v)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

twoTrigger = undefined::Two String (String, [String], Int)

data Three a b c = Three a b c deriving ( Eq, Show )

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

threeTrigger = undefined::Three String [Int] (String, [String], Int)

data Three' a b = Three' a b b deriving ( Eq, Show )

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a f g) <*> (Three' a' b c) = Three' (a <> a') (f b) (g c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

three'Trigger = undefined::Three' [Int] (String, [String], Int)

data Four' a b = Four' a a a b deriving ( Eq, Show )
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a a' a'' f) <*> (Four' b b' b'' c) = Four' (a <> b) (a' <> b') (a'' <> b'') (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

four'Trigger = undefined::Three' [Int] (String, [String], Int)

main :: IO ()
main = do
        quickBatch $ applicative trigger
        quickBatch $ applicative pairTrigger
        quickBatch $ applicative twoTrigger
        quickBatch $ applicative threeTrigger
        quickBatch $ applicative three'Trigger
        quickBatch $ applicative four'Trigger
