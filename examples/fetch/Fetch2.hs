module Fetch2 where

data Request a = Request a

data Fetch a = Done a | forall r. Blocked (Request r) (r -> Fetch a)

instance Functor Fetch where
  fmap f (Done x) = Done $ f x
  fmap f (Blocked r c) = Blocked r $ fmap f c

instance Monad Fetch where
  return = pure

  Done a >>= k = k a
  Blocked r c >>= k = Blocked r (c >>= k)

instance Applicative Fetch where
  pure = Done

  Done g <*> Done y = Done $ g y
  Done g <*> Blocked s d = Blocked s $ g <$> d
  Blocked r c <*> Done y = Blocked r $ c <*> Done y
  Blocked r c <*> Blocked s d = Blocked (r,s) $ c <*> d

runFetch :: Fetch a -> a
runFetch (Done a) = a
runFetch (Blocked r c) = runFetch c

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do f <- mf; x <- mx; return $ f x

