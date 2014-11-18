module Chap01 where

import Data.List

minfrom a (n, xs) | n == 0     = a
                  | m == b - a = minfrom b (n - m, vs)
                  | otherwise  = minfrom a (m, us)
  where (us, vs) = partition (< b) xs
        b = a + 1 + n `div` 2
        m = length us

minfree xs = minfrom 0 (length xs, xs)
