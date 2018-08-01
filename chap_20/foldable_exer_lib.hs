import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
--elem' = any . (==)
--elem' x = getAny . foldMap (Any . (x ==)) 
--elem' = (getAny .) . foldMap . (Any .) . (==)
--elem' x = foldr (\a b -> (x == a) || b) False
elem' = flip foldr False . (((||) .) . (==))

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
--minimum' = Just . foldr1 min
minimum' = foldr mf Nothing
  where
     mf x m = Just (case m of
                       Nothing -> x
                       Just y  -> min x y)
    
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr mf Nothing
  where 
    mf x m = Just (case m of 
                     Nothing -> x
                     Just y -> max x y)

null' :: (Foldable t) => t a -> Bool
null' = (0 == ) . foldr (\_ x -> x + 1) 0 

length' :: (Foldable t) => t a -> Int
length' = foldr ((+) . (const 1)) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--foldMap f = foldr (mappend . f) mempty
foldMap' = flip foldr mempty . (mappend .)


data Constant a b =
  Constant a

instance Monoid a => Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

data Two a b = Two a b
