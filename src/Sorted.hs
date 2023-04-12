{- Code is adapted (and really, mostly just copied) from Matt Noonan's paper "Ghosts of Departed Proofs"-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}

module Sorted
  ( SortedBy, sortBy, mergeBy
  ) where

import The
import Named

import Data.Coerce (coerce)
import Data.List qualified as L
import Data.List.Utils qualified as U

-- newtype Named name a = Named a 
newtype SortedBy comp a = SortedBy a
{-
  also remb: 
    type a ~~ name = Named a 
  read: 
    values of type a with name n
-}
instance The (SortedBy comp a) a
-- instance The ((a -> a -> Ordering) ~~ comp) (a -> a -> Ordering)

sortBy :: ((a -> a -> Ordering) ~~ comp)
       -> [a]
       -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

mergeBy :: ((a -> a -> Ordering) ~~ comp)
        -> SortedBy comp [a]
        -> SortedBy comp [a]
        -> SortedBy comp [a]
mergeBy comp xs ys =
  coerce (U.mergeBy (the comp) (the xs) (the ys))
