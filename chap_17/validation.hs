module Validation where

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

main :: IO ()
main = quickBatch $ applicative trigger
