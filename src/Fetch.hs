import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))
import Data.IORef
import Data.Sequence

{-# LANGUAGE arrows #-}

module Fetch where

type PostId = Int
type PostInfo = Int
type PostContent = Int

data Request b a where
  FetchPosts :: Request () [PostId]
  FetchPostInfo :: Request PostId PostInfo
  FetchPostContent :: Request PostId PostContent
  FetchPostViews :: Request PostId Int

-- I'm really not sure how this should work
data BlockedRequest = forall b a. BlockedRequest (Request b a) (IORef (FetchStatus b a))
data FetchStatus b a = NotFetched b | FetchSuccess a

data Fetch b a = Ready (b -> a) | Blocked (Seq BlockedRequest) (Fetch b a)
-- data Fetch b a = b -> Ready a | Blocked (Fetch b a)

instance Category Fetch where
  id = Ready id
--  Ready f . Ready g = Ready $ f . g
--  Blocked j . Ready g = Blocked $ j . (Ready g)
--  Ready f . Blocked k = 

instance Arrow Fetch where
  arr = Ready

  Ready f   *** Ready g   = Ready (\(x, y) -> (f x, g y))
  Ready f   *** Blocked k = Blocked $ Ready f *** k
  Blocked j *** Ready g   = Blocked $ j *** Ready g
  Blocked j *** Blocked k = Blocked $ j *** k

runFetch :: Fetch b a -> b -> a
runFetch (Ready f) = f
runFetch (Blocked k) = runFetch k


example :: Request () PostContent
example = proc () -> do
  ids <- getPostIds -< ()
  let i = head ids
  getPostContent &&& getPostViews -< i

