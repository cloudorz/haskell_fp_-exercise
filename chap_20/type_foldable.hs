
data Constant a b = Constant a
data Two a b = Two a b
data Three a b c = Three a b c
data Three' a b = Three' a b b
data Four' a b = Four' a b b b

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Foldable (Three' a) where
  foldMap f (Three _ b b') = f c

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)



