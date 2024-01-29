module Automata.Internal where

import Data.Set
  ( Set
  , union
  , member )
import qualified Data.Set as Set
  ( empty
  , singleton
  , toList
  , filter
  , insert )

computeClosure :: Ord a => (a -> Set a) -> Set a -> Set a
computeClosure f xs = aux (Set.toList xs, Set.empty) where
  aux ([], acc) = acc
  aux (h:ts, acc) = aux (Set.toList newNexts ++ ts, Set.insert h (acc `union` newNexts)) where
    nexts = f h
    newNexts = Set.filter (not . (`member` acc)) nexts
