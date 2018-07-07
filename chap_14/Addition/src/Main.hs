module Main where

import Test.Hspec
import Test.QuickCheck

multiBy :: (Eq a, Num a) => a -> a -> a
multiBy x y = go x y 0
  where go m n count 
          | n == 0 = count
          | m == 0 = count
          | otherwise = go m (n - 1) (count + m)

main :: IO ()
main = hspec $ do
  describe "multiBy" $ do 
    it "multiBy 3 4 is equal to 12" $ do 
      multiBy 3 4 `shouldBe` 12
    it "multiBy 0 4 is equal to 0" $ do 
      multiBy 0 4 `shouldBe` 0
    it "multiBy 4 0 is equal to 0" $ do 
      multiBy 4 0`shouldBe` 0
    it "x + 1 is alwas greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)
