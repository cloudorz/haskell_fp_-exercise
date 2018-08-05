module Trivial where

import Data.Semigroup
import qualified Data.Monoid as N
import Test.QuickCheck

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where 
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance (Semigroup a, Monoid a) => Monoid (Identity a) where 
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj $ a && b

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> (choose (True, False))

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 8
data Or a b = Fst a | Snd b deriving ( Eq, Show )

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  _ <> (Snd b) = Snd b
  _ <> (Fst a) = Fst a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-- 9
newtype Combine a b = 
  Combine { unCombine :: (a -> b) } 

instance Semigroup b => Semigroup (Combine a b) where
  a <> b = Combine $ \x -> unCombine a x <> unCombine b x

-- 10
newtype Comp a = 
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  (Comp a) <> (Comp b) = Comp $ \v -> a v <> b v

-- 11

data Validation a b = Fail a | Suc b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  (Fail a) <> (Fail b) = Fail $ a <> b
  _ <> (Fail b) = Fail b
  (Fail b) <> _ = Fail b

newtype AccumulateRight a b = AccumulateRight (Validation a b) deriving (Eq, Show)
instance Semigroup b => Semigroup (AccumulateRight a b) where
  (AccumulateRight (Suc b)) <> (AccumulateRight (Suc b')) = AccumulateRight (Suc $ b <> b')
  (AccumulateRight (Fail a)) <> _ = AccumulateRight (Fail a)
  _ <> (AccumulateRight (Fail a)) = AccumulateRight (Fail a)

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  (AccumulateBoth (Suc b)) <> (AccumulateBoth (Suc b')) = AccumulateBoth (Suc $ b <> b')
  (AccumulateBoth (Fail a)) <> (AccumulateBoth (Fail a')) = AccumulateBoth (Fail $ a <> a')
  (AccumulateBoth (Fail a)) <> _ = AccumulateBoth (Fail a)
  _ <> (AccumulateBoth (Fail a)) = AccumulateBoth (Fail a)
-- 
-- testing
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c) 

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty N.<> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a N.<> mempty) == a


-- main
main :: IO () 
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (semigroupAssoc :: OrAssoc)
