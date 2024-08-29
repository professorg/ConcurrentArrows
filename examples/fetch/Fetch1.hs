module Fetch1 where

data Fetch a = Done a | Blocked (Fetch a)
  deriving Show

instance Functor Fetch where
  fmap f (Done x) = Done $ f x
  fmap f (Blocked c) = Blocked $ fmap f c

instance Monad Fetch where
  return = pure

  Done a >>= k = k a
  Blocked c >>= k = Blocked (c >>= k)

instance Applicative Fetch where
  pure = Done

  Done g <*> Done y = Done $ g y
  Done g <*> Blocked d = Blocked $ g <$> d
  Blocked c <*> Done y = Blocked $ c <*> Done y
  Blocked c <*> Blocked d = Blocked $ c <*> d

runFetch :: Fetch a -> a
runFetch (Done a) = a
runFetch (Blocked c) = runFetch c

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do f <- mf; x <- mx; return $ f x

