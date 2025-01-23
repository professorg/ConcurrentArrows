import Data.IORef
import Data.Sequence

module Fetch3 where

type PostId = Int
type PostInfo = Int
type PostContent = Int

data Request a where
  FetchPosts :: Request [PostId]
  FetchPostInfo :: PostId -> Request PostInfo
  FetchPostContent :: PostId -> Request PostContent
  FetchPostViews :: PostId -> Request Int

-- Existential quantified for use in heterogenous list
-- Blocked request has its own IORef where it places its result
data BlockedRequest = forall a. BlockedRequest (Request a) (IORef (FetchStatus a))

-- Computation which will eventually result in an a
data FetchStatus a = NotFetched | FetchSuccess a -- this is just Maybe

data Result a = Done a
-- Heterogenous sequence of computations which have to run to produce an a
-- Fetch a is a continuation which can run after each blocked request runs
              | Blocked (Seq BlockedRequest) (Fetch a) -- Sequence of BlockedRequests which must run before an 'a' is reached

newtype Fetch a = Fetch { unFetch :: IO (Result a) }

instance Functor Fetch where
  fmap f (Fetch x) = Fetch (fmap f x)

instance Monad Fetch where
  return = pure

  Fetch m >>= k = Fetch $ do
    r <- m
    case r of
      Done a -> unFetch $ k a
      Blocked c -> return $ Blocked br (c >>= k)

instance Applicative Fetch where
  pure a = Fetch $ pure (Done a)

  Fetch f <*> Fetch x = Fetch $ do
    f' <- f
    x' <- x
    case (f', x') of
      (Done g,        Done y)        -> return $ Done (g y)
      (Done g,        Blocked br c)  -> return $ Blocked br (g <$> c)
      (Blocked br c,  Done y)        -> return $ Blocked br (c <*> return y)
      (Blocked br1 g, Blocked br2 f) -> return $ Blocked (br1 <> br2) (c <*> d)

dataFetch :: Request a -> Fetch a
dataFetch request = Fetch $ do
  box <- newIORef NotFetched
  let br = BlockedRequest request box
  let cont = Fetch $ do
        FetchSuccess a <- readIORef box
        return $ Done a
  return $ Blocked (singleton br) cont

getPostIds = dataFetch FetchPosts
getPostInfo = dataFetch . FetchPostInfo
getPostContent = dataFetch . FetchPostContent
getPostViews = dataFetch . FetchPostViews

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = do f <- mf; x <- mx; return $ f x

-- This is where concurrency and batching is introduced
fetch :: [BlockedRequest] -> IO ()

runFetch :: Fetch a -> IO a
runFetch (Fetch h) = do
  r <- h
  case r of
    Done a -> return a
    Blocked br cont -> do
      fetch (toList br)
      runFetch cont

