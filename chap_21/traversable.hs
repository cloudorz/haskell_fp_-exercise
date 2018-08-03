import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Arbitrary

newtype Identity a = Identity a 
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Traversable Identity where 
  traverse f (Identity a) = Identity <$> f a

identityTrigger = undefined :: Identity (Int, Int, [Int])

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b `mappend` f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance Traversable (Three' a) where
  traverse f (Three' a b b') = Three' a <$> f b <*> f b'

three'Trigger = undefined :: (Three' (Int, Int, [String]) (String, Int, [Int]))

newtype Constant a b =
  Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = pure (Constant a)

instance (Arbitrary a) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

constantTrigger = undefined :: (Constant (String, Int, [Int]) (Int, Int, String))

data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance (Eq a) => EqProp (Optional a) where
  (=-=) = eq

optionalTrigger = undefined :: (Optional (Int, String, [Int]))

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a as) = f a `mappend` (foldMap f as)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> (traverse f as)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = oneof [pure Nil, Cons <$> arbitrary <*> arbitrary]

instance (Eq a) => EqProp (List a) where
  (=-=) = eq

listTrigger = undefined :: List (Int, String, [String])

data S n a = S (n a) a deriving (Eq, Show)

instance Traversable n => Traversable (S n) where 
  traverse f (S s a) = S <$> traverse f s <*> f a

instance Functor n => Functor (S n) where
  fmap f (S s a) = S (fmap f s) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S s a) = foldMap f s `mappend` f a

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
  (=-=) = eq

sTrigger = undefined :: (S [] (Int, Int, [String]))

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a) 
  deriving (Eq, Show)

instance Functor Tree where 
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right) 

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = (foldMap f left) `mappend` f a `mappend` (foldMap f right)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = Node <$> (traverse f left) <*> f a <*> (traverse f right)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [pure Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

treeTrigger = undefined :: (Tree (String, String, [Int]))

main :: IO ()
main = do 
  quickBatch (traversable identityTrigger)
  quickBatch (traversable three'Trigger)
  quickBatch (traversable constantTrigger)
  quickBatch (traversable optionalTrigger)
  quickBatch (traversable listTrigger)
  quickBatch (traversable sTrigger)
  quickBatch (traversable treeTrigger)
