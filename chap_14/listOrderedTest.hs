-- for any list you apply sort to
-- this property should hold 
import Data.List (sort)
import Test.QuickCheck

listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
  snd $ foldr go (Nothing, True) xs 
  where go _ status@(_, False) = status 
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

main :: IO ()
main = do 
  quickCheck (\xs -> listOrdered (sort (xs :: [Int])))
  quickCheck (\x y z -> plusAssociative (x :: Int) (y :: Int) (z :: Int))
  quickCheck (\x y -> plusCommutative (x :: Int) (y :: Int))
