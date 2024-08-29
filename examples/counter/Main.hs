{-# LANGUAGE GADTs              #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Arrow.Freer.FreerArrow
import Control.Arrow.State.ArrowState
import Prelude hiding (lookup)
import Data.Map

class Counter a where
  zero :: a
  inc :: a -> a

type Counters ar k s b a =
  Counter s
    => StateA (Map k s) b a 

touch :: (Ord k, Counter s)
  => Counters ar k s k s
touch = proc x -> do
  old_map <- get -< ()
  let y = findWithDefault zero x old_map
  put -< adjust inc x old_map
  returnA -< y

main :: IO ()
main = pure ()

