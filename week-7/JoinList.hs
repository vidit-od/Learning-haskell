{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Buffer
import Sized
import Editor


data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2 

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty     = Nothing

indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing

indexJ i (Append m jl1 jl2)
  | i < 0 || i >= root = Nothing
  | i < left           = indexJ i jl1
  | otherwise          = indexJ (i - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl

dropJ _ Empty = Empty

dropJ _ (Single _ _) = Empty

dropJ n (Append m jl1 jl2)
  | n >= root = Empty
  | n < left  = dropJ n jl1 +++ jl2
  | otherwise = dropJ n jl1 +++ dropJ (n - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty

takeJ _ Empty = Empty

takeJ _ jl@(Single _ _) = jl

takeJ n jl@(Append m jl1 jl2)
  | n >= root = jl
  | n < left  = takeJ n jl1
  | otherwise = takeJ n jl1 +++ takeJ (n - left) jl2
  where root = getSize . size $ m
        left = getSize . size . tag $ jl1


main :: IO()
main  = do 
    putStr (printResults appendTest)
    putStr (printResults tagTest)
