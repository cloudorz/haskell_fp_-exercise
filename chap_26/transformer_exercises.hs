-- EitherT
newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where 
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where 
  pure = EitherT . pure . pure
  (EitherT mef) <*> (EitherT mea) = EitherT $ fmap (<*>) mef <*> mea

instance Monad m => Monad (EitherT e m) where 
  return = pure
  (EitherT mea) >>= f = EitherT $ do 
    a <- mea
    case a of
         Left e -> return (Left e)
         Right v -> runEitherT (f v)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => 
           (a -> m c) 
        -> (b -> m c)
        -> EitherT a m b
        -> m c 
eitherT f g (EitherT mab) = do 
  v <- mab
  case v of 
    Left a -> f a
    Right b -> g b

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT f) <*> (ReaderT a) = ReaderT $ fmap (<*>) f <*> a

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma) >>= f = ReaderT $ \r -> do
                                        a <- rma r
                                        runReaderT (f a) r
  --(ReaderT rma) >>= f = ReaderT $ \r -> join $ (fmap runReaderT (fmap f (rma r))) <*> pure r

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }
  
instance Functor m => Functor (StateT s m) where
  --fmap f (StateT sma) = StateT $ (fmap . fmap) f sma
  fmap f (StateT sma) = StateT $ \s -> fmap (\(a, s) -> (f a, s)) (sma s)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  --(StateT f) <*> (StateT sma) = StateT $ fmap (<*>) f <*> sma
  (StateT f) <*> (StateT sma) = StateT $ \s -> do
                                                 (fa, s') <- f s
                                                 (a, s'') <- sma s'
                                                 return (fa a, s'')
  
instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \s -> do
                                      (a, s') <- sma s
                                      runStateT (f a) s'
