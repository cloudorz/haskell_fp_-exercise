import Data.Tuple

newtype State s a =
  State { runState :: s -> (a, s) }

instance Functor (State s) where
  --fmap f (State g) = State $ flip (,) <*> (f . fst . g) -- f . fst . g :: s -> b, flip (,) :: s -> b -> (b, s), f :: ((->) s)
  --fmap f (State g) = State $ do 
   --                            (a, s) <- g
    --                           return (f a, s)
  --fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')
  fmap f (State g) = State $ uncurry ((,) . f) . g

instance Applicative (State s) where
  --pure a = State $ \s -> (a, s)
  pure a = State $ (,) a
  --(State f) <*> (State g) = State $ do 
    --                                 (ff, s) <- f
     --                                 let (a, s') = g s
      --                                return (ff a, s')
  --(State f) <*> (State g) = State $ \s -> let (ff, s') = f s
   --                                           (a, s'') = g s'
    --                                      in (ff a, s'')
  (State f) <*> (State g) = State $ swap . uncurry fmap . fmap swap . fmap g . f
  
  --(State f) <*> (State g) = State $ swap <$> ((((<*>) . swap) <$> f) <*> (swap <$> g))
instance Monad (State s) where
  return = pure 
  --(State f) >>= g = State $ do 
   --                          (a, s) <- f
   --                         runState (g a)
  
  (State f) >>= g = State $ uncurry (runState . g) . f

-- 1
get :: State s s
get = State $ \s -> (s, s)

-- 2
put :: s -> State s ()
--put s = State $ const ((), s)
put = State <$> const . (,) ()

-- 3
exec :: State s a -> s -> s
exec (State sa) s = snd $ sa s

-- 4
eval :: State s a -> s -> a 
eval (State sa) = fst . sa

-- 5
modify :: (s -> s) -> State s () 
--modify f = State $ \s -> ((), f s)
modify = State <$> fmap ((,) ())
