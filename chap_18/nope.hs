module Ex where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Arbitrary

-- 18.7.1
data Nope a = NopeDotJpg deriving ( Show, Eq )

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
   (=-=) = eq

-- 187.7.2
data PhhhbbtttEither b a = 
    Left a
  | Right b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Ex.Left a) = Ex.Left $ f a 
  fmap _ (Ex.Right b) = Ex.Right b

instance Applicative (PhhhbbtttEither b) where
  pure = Ex.Left 
  (Ex.Left f) <*> (Ex.Left a) = Ex.Left $ f a
  (Ex.Right b) <*> _ = Ex.Right b
  _ <*> (Ex.Right b) = Ex.Right b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Ex.Left a) >>= f = f a
  (Ex.Right b) >>= _ = Ex.Right b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Ex.Left <$> arbitrary, Ex.Right <$> arbitrary]

instance Eq a => EqProp (PhhhbbtttEither b a) where 
  (Ex.Left a) =-= (Ex.Left b) = eq a b
  _ =-= _ = property True

-- 18.7.3
newtype Identity a = Identity a 
    deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity $ f a
  -- (Identity f) <*> a = fmap f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 18.7.4
j :: Monad m => m (m a) -> m a
j a = a >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = f <$> a <*> b

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>) 

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (a:b) f = (:) <$> (f a) <*> (meh b f)

flipType :: Monad m => [m a] -> m [a]
flipType = flip meh id
