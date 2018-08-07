{-# LANGUAGE InstanceSigs #-}

import Control.Comonad.Identity

newtype Reader s a = Reader { runReader :: s -> a }

ask :: Reader a a 
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a 
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where 
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where 
  return = pure
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r


