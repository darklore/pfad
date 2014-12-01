module Chap05 where

import Data.List

-- data A = A { val :: Int } deriving Show
type A = Int

-- data Label a = Label (a, (Int, Int))
type Label a = (a, (Int, Int))
instance (Eq a) => Eq (Label a) where
  (x, _) == (y, _) = x == y

instance (Ord a) => Ord (Label a) where
  compare (x, _) (y, _)
    | x == y = EQ
    | x < y = LT
    | x > y = GT

add :: A -> A -> A
add x y = x + y

sub :: A -> A -> A
sub x y = x - y

negate :: A -> A
negate x = x * (-1)

subs :: [A] -> [A] -> [Label A]
subs xs ys = [ (x `sub` y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..] ]

sortsubs :: [A] -> [A] -> [Label A]
sortsubs xs ys = sort (subs xs ys)

sortsums xs ys = map fst (sortsubs xs (map Chap05.negate ys))
