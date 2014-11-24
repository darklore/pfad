module Chap03 where

invert f z = [(x, y) | x <- [0..z], y <- [0..z], f (x , y) == z]

invert2 f z = [(x, y) | x <- [0..z], y <- [0..z - x], f (x , y) == z]

find (u, v) f z = [(x, y) | x <- [u..z], y <- [v, v - 1..0], f (x, y) == z]

invert3 f z = find (0, z) f z

find2 (u, v) f z
  | u > z || v < 0 = []
  | zd < z         = find (u + 1, v) f z
  | zd == z        = (u, v) : find (u + 1, v - 1) f z
  | zd > z         = find (u, v - 1) f z
  where zd = f (u, v)

invert4 f z = find2 (0, z) f z

bsearch g (a, b) z
 | a + 1 == b = a
 | g m <= z = bsearch g (m, b) z
 | otherwise = bsearch g (a, m) z
  where m = (a + b ) / 2
