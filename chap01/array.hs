module Chap01 where
import Data.Array
import Data.Array.ST

input :: [Int]
input = [23,56,234,6,2,4,31,542,34,25,23, 0, 1]

sub us vs = filter (`notElem` vs) us

minfree xs = head (sub [0..] xs)

--
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n)
               (zip (filter (<= n) xs) (repeat True))
  where n = length xs

minfree2 = search . checklist


----
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
  where n = maximum xs

sort xs = concat [ replicate k x | (x, k) <- assocs (countlist xs) ]

checklist2 xs = runSTArray (do
                               {a <- newArray (0,n) False;
                                sequence [ writeArray a x True | x <- xs,x <= n];
                                return a})
  where n = length xs
