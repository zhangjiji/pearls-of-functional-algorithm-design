import Data.Array
import Data.List

(\\.) :: Eq a => [a] -> [a] -> [a]
us \\. vs = filter (`notElem` vs) us

minfree1 :: [Int] -> Int
minfree1 xs = head ([1..] \\. xs)

minfree2 :: [Int] -> Int
minfree2 = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n) (zip (filter (<= n) xs) (repeat True))
  where n = length xs

minfree3 :: [Int] -> Int
minfree3 xs = if null ([0..b-1] \\. us)
              then head ([b..] \\. vs)
              else head ([0..] \\. us)
  where (us,vs) = partition (<b) xs
        b = (length xs) `div` 2

minfree4 :: [Int] -> Int
minfree4 xs = if length us == b
              then head ([b..] \\. vs)
              else head ([0..] \\. us)
  where (us,vs) = partition (<b) xs
        b = (length xs) `div` 2

minfrom a xs
  | null xs = a
  | length us == b-a = minfrom b vs
  | otherwise = minfrom a us
  where (us, vs) = partition (<b) xs
        n = length xs
        b = a + 1 + n `div` 2

minfree5 xs = minfrom' 0 (length xs, xs)
minfrom' a (n, xs)
  | n == 0 = a
  | m == b - a = minfrom' b (n-m, vs)
  | otherwise = minfrom' a (m, us)
  where (us, vs) = partition (<b) xs
        b = a + 1 + n `div` 2
        m = length xs
