import           Data.List

takeInt :: Int -> [a] -> [a]
takeInt 0 xs = []
takeInt m (x:xs) = x : takeInt (m - 1) xs

dropInt :: Int -> [a] -> [a]
dropInt 0 xs = xs
dropInt m (x:xs) = dropInt (m - 1) xs

sumInt :: [Int] -> Int
sumInt [] = 0
sumInt (x : xs) = x + (sumInt xs )

scanSum :: [Int] -> [Int]
scanSum xs = go xs 0
    where go [] m = []
          go (x : xs)  m = (x+m) : go xs (x+m)

diffs :: [Int] -> [Int]
diffs [] = []
diffs [x] = []
diffs (x : y : ys) = (abs (x-y)) : diffs (y:ys)

--- Run Length Encoding
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map f $ group xs
    where f xs = (length xs, head xs)

decode :: Eq a => [(Int, a)] -> [a]
decode xs = concat $ map f xs
    where f (m, x) = replicate m x
