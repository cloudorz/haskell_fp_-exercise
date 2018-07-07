{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a deriving ( Eq, Show )

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

data Pair a = Pair a a deriving ( Eq, Show )
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

data Three a b c = Three a b c deriving ( Eq, Show )
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

data Three' a b = Three' a b b deriving ( Eq, Show )

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three' a b c)

data Four' a b = Four' a a a b deriving ( Eq, Show )
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four' a b c d)

newtype K a b = K a deriving ( Eq, Show )
instance Functor (K a) where
  fmap _ (K a) = K a

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

instance Arbitrary b => Arbitrary (Flip K a b) where
  arbitrary = Flip <$> arbitrary

type IntToInt = Fun Int Int
type IntIdentity = Identity Int -> IntToInt -> IntToInt -> Bool
type IntPair = Pair Int -> IntToInt -> IntToInt -> Bool
type IntThree = Three Int Int Int -> IntToInt -> IntToInt -> Bool
type IntThree' = Three' Int Int -> IntToInt -> IntToInt -> Bool
type IntFour' = Four' Int Int -> IntToInt -> IntToInt -> Bool
type IntKFlip = Flip K Int Int -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do 
  quickCheck (functorCompose' :: IntIdentity)
  quickCheck (functorCompose' :: IntPair)
  quickCheck (functorCompose' :: IntThree)
  quickCheck (functorCompose' :: IntThree')
  quickCheck (functorCompose' :: IntFour')
  quickCheck (functorCompose' :: IntKFlip)
