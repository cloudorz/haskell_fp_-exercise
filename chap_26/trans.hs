import Control.Monad.Trans.Class
import Control.Monad (liftM)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> (\x -> (x, s)) <$> m
