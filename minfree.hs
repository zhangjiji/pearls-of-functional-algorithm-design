import Data.Array
import Data.List

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

minfree1 :: [Int] -> Int
minfree1 xs = head ([1..] \\ xs)

minfree2 :: [Int] -> Int
minfree2 = search . checklist

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0,n) (zip (filter (<= n) xs) (repeat True))
  where n = length xs


