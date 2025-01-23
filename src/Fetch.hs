{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}

module Fetch where

import Control.Category
import Control.Arrow
import Prelude hiding (id, (.))
import Data.Profunctor


import Control.Arrow.Freer.FreerArrowOps
import qualified Control.Arrow.Freer.FreerWeakArrow as W
import qualified Data.Map as Map


type PostId = Int
type PostInfo = Int
type PostContent = Int

data Request b a where
  FetchPosts :: Request () [PostId]
  FetchPostInfo :: Request PostId PostInfo
  FetchPostContent :: Request PostId PostContent
  FetchPostViews :: Request PostId Int
  UpdatePostViews :: Request (PostId, Int) ()

data RequestLog = FetchPostsLog [PostId]
                | FetchPostInfoLog PostId PostInfo
                | FetchPostContentLog PostId PostContent
                | FetchPostViewsLog PostId Int
                | UpdatePostViewsLog PostId Int
  deriving Show


type Fetch = FreerArrowOps Request

-- FetchPosts >>> arr hd >>> (arr id &&& (FetchPostViews >>> arr inc)) >>> UpdatePostViews
-- FetchPosts >>> arr hd >>> arr dup >>> (arr id *** (FetchPostViews >>> arr inc)) >>> UpdatePostViews
-- ()        [PostId]    PostId    (PostId, PostId)                          (PostId, Int)      ()
-- FetchPosts -> FetchPostViews -> UpdatePostViews

-- I'm really not sure how this should work
--data BlockedRequest = forall b a. BlockedRequest (Request b a) (IORef (FetchStatus a))
--data FetchStatus a = NotFetched | FetchSuccess a

-- > it becomes hard to maintain the connections between multiple result types r
-- > and their continuations.
--
-- I think this is exactly what I'll have to do, or I need to otherwise
-- linearize the IORef updates
--
-- ()      [PostId]    PostId           (PostId, PostId)             (PostInfo, PostContent)
-- FetchPosts >>> arr hd >>> arr (\x -> (x, x)) >>> (FetchPostInfo *** FetchPostContent)
-- FetchPosts >>> arr hd >>> (FetchPostInfo &&& FetchPostContent)
-- cells:
-- 1: _ : ()
-- 2: _ : [PostId]
-- 3: _ : PostId
-- 4: _ : PostInfo
-- 5: _ : PostContent
--
-- FetchPosts -> (FetchPostInfo, FetchPostContent)
--
--              ,-> 4
-- 1 -> 2 -> 3 <
--              '-> 5
--
-- maybe I just compile the computation into the monad representation?

-- data BlockedRequest = forall b a. BlockedRequest (Request b a) (IORef (FetchStatus a))
-- data FetchStatus a = NotFetched | FetchSuccess a

-- data Result a = Done a | Blocked (Seq BlockedRequest) (Fetch b a)

-- type IOResult a = IO (Result a)
  
-- Maybe use initial encoding instead
-- data Fetch b a = F { un :: Kleisli IOResult b a }
-- data Fetch b a = Ready (b -> a) | Blocked (Seq BlockedRequest) (Fetch b a)
-- data Fetch b a = b -> Ready a | Blocked (Fetch b a)

-- data Fetch b a where

-- instance Category Fetch where

-- TODO: Instead of interpreting to Kleisli IO try
-- interpreting to an arrow that resembles state
-- for a database that logs writes
--
-- TODO: version numbers for resolving merge conflicts
type Database = Map.Map PostId (PostInfo, PostContent, Int)
newtype LogState i r = LogState {unLogState :: i -> Database -> ( [[RequestLog]], Database, r )}
-- e.g. [[GetPostId _ _, PostId], [Fetch1, Fetch2], ...]
-- log (e :: Request b a) :: (RequestLog b a)

--       B > C
--   A <           > F
--       D > E > G
--
--  [[A]] + (([[B]] + [[C]]) * ([[D]] + [[E]])) + [[F]]
--
-- [[A], [B, D], [C, E], [F]] Batching
-- [[A], [B, D], [C, E], [G], [F]] Batching
--
-- [[A], [B, C], [D, E], [F]] Sequencing/Threading
--
discard :: Arrow ar => ar a ()
discard = arr (const ())
--
--    discard >>> B >>> discard >>> C
-- proc () -< do
--   B
--   C
--    discard >>> B >>> (id &&& (discard >>> C))
-- ~
--    (B &&& C)

instance Functor (LogState i) where
  fmap f (LogState g) = LogState $ \x db ->
    let (log', db', y) = g x db in
      (log', db', f y)

instance Category LogState where
  id = LogState $ \x db -> ([], db, x)
  LogState f . LogState g = LogState $ \x db ->
    let (log1, db1, y) = g x db in
    let (log2, db2, z) = f y db1 in
    (log1 <> log2, db2, z)

instance Arrow LogState where
  arr f = LogState $ \x db -> ([], db, f x)

  LogState f *** LogState g = LogState $ \(x, y) db ->
    let (log1, db1, z) = f x db
        (log2, db2, w) = g y db in
      -- TODO: what does db1 <> db2 do?
      (log1 <> log2, db1 <> db2, (z, w))

instance ArrowChoice LogState where
  LogState f +++ LogState g = LogState $ \x db ->
    case x of
      Left y -> Left <$> f y db
      Right y -> Right <$> g y db

runFetchLogState :: Fetch :-> LogState
runFetchLogState (One (W.Hom f)) = arr f
runFetchLogState (One (W.Comp f x y)) =
  runFetchLogState (One (W.Hom f)) >>>
  dataFetchLogState x >>>
  runFetchLogState (One y)
runFetchLogState (Seq f g) = runFetchLogState f >>> runFetchLogState g
runFetchLogState (And f g) = runFetchLogState f *** runFetchLogState g
runFetchLogState (Or f g) = runFetchLogState f +++ runFetchLogState g

dataFetchLogState :: Request :-> LogState
dataFetchLogState FetchPosts = LogState $ \_ db ->
  let postids = Map.keys db in
    ([[FetchPostsLog postids]], db, postids)
dataFetchLogState FetchPostInfo = LogState $ \i db ->
  let entry = Map.lookup i db in
  -- TODO: put into option or something
  let Just (info, _, _) = entry in
    ([[FetchPostInfoLog i info]], db, info)
dataFetchLogState FetchPostContent = LogState $ \i db ->
  let entry = Map.lookup i db in
  -- TODO: put into option or something
  let Just (_, content, _) = entry in
    ([[FetchPostContentLog i content]], db, content)
dataFetchLogState FetchPostViews = LogState $ \i db ->
  let entry = Map.lookup i db in
  -- TODO: put into option or something
  let Just (_, _, views) = entry in
    ([[FetchPostViewsLog i views]], db, views)
dataFetchLogState UpdatePostViews = LogState $ \(i, n) db ->
  let db' = Map.adjust (\(info, c, _) -> (info, c, n)) i db in
    ([[UpdatePostViewsLog i n]], db', ())

runFetch :: Fetch :-> Kleisli IO
runFetch (One (W.Hom f)) = Kleisli $ pure <$> f
runFetch (One (W.Comp f x y)) =
  runFetch (One (W.Hom f)) >>>
  dataFetch x >>>
  runFetch (One y)
runFetch (Seq f g) = runFetch f >>> runFetch g
runFetch (And f g) = runFetch f *** runFetch g
runFetch (Or f g) = runFetch f +++ runFetch g

dataFetch :: Request :-> Kleisli IO
dataFetch FetchPosts = Kleisli . const . pure $ [0, 1]
dataFetch FetchPostInfo = Kleisli . fmap pure $ \case
  0 -> 2
  1 -> 3
  _ -> 0
dataFetch FetchPostContent = Kleisli . fmap pure $ \case
  0 -> 5
  1 -> 7
  _ -> 0
dataFetch FetchPostViews = Kleisli . fmap pure $ \case
  0 -> 11
  1 -> 13
  _ -> 0
dataFetch UpdatePostViews = Kleisli . const . pure $ ()

getPosts = embed FetchPosts
getPostInfo = embed FetchPostInfo
getPostContent = embed FetchPostContent
getPostViews = embed FetchPostViews
updatePostViews = embed UpdatePostViews

example :: Fetch () PostContent
example = getPosts >>> arr head >>> getPostContent

exampleDb :: Database
exampleDb =
  Map.fromAscList [ (0, (2, 5, 11)),
                    (1, (3, 7, 13))
                  ]

example2 :: Fetch () PostContent
example2 = getPosts >>> ((arr head >>> getPostContent) &&& (arr last >>> getPostContent)) >>> arr (uncurry (+))



example3 :: Fetch () ()
example3 = getPosts
  >>> arr head
  >>>
    (&&&)
      id
      (getPostViews >>> arr (+ 1))
  >>> updatePostViews

example4 :: Fetch () ()
example4 = getPosts
  >>> arr head
  >>>
    (&&&)
      id
      (getPostViews >>> arr (+ 1))
  >>> (updatePostViews &&& updatePostViews)
  >>> arr (const ())

newtype Const t b a = Const { unConst :: t }

-- Const True :: Const Bool

instance Monoid t => Category (Const t) where
  id = Const mempty

  Const x . Const y = Const (x <> y)

instance Monoid t => Arrow (Const t) where


data WriterStatus = Writer | NoConflict | Conflict
  deriving Show

parJoin :: WriterStatus -> WriterStatus -> WriterStatus
parJoin Writer Writer = Conflict
parJoin Conflict _ = Conflict
parJoin _ Conflict = Conflict
parJoin NoConflict x = x
parJoin x NoConflict = x

seqJoin :: WriterStatus -> WriterStatus -> WriterStatus
seqJoin Conflict _ = Conflict
seqJoin _ Conflict = Conflict
seqJoin Writer _ = Writer
seqJoin _ Writer = Writer
seqJoin NoConflict NoConflict = NoConflict

isWriter :: Request :-> Const WriterStatus
isWriter UpdatePostViews = Const Writer
isWriter _ = Const NoConflict

hasWriteConflict :: Fetch :-> Const WriterStatus
hasWriteConflict (One (W.Hom _)) = Const NoConflict
hasWriteConflict (One (W.Comp _ x y)) = Const $ seqJoin (unConst $ isWriter x) (unConst $ hasWriteConflict (One y))
hasWriteConflict (Seq f g) = Const $ seqJoin (unConst $ hasWriteConflict f) (unConst $ hasWriteConflict g)
hasWriteConflict (And f g) = Const $ parJoin (unConst $ hasWriteConflict f) (unConst $ hasWriteConflict g)
hasWriteConflict (Or f g) = Const $ parJoin (unConst $ hasWriteConflict f) (unConst $ hasWriteConflict g)

