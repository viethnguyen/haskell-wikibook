doubleFactorial :: Int -> Int
doubleFactorial 1 = 1
doubleFactorial 2 = 2
doubleFactorial n = n * doubleFactorial (n-2)

mult _ 0 = 0
mult n 1 = n
mult n m = (mult n (m - 1)) + n

power _ 0 = 1
power n 1 = n
power n m = (power n (m -1)) * n

plusOne x = x + 1

addition 0 y = y
addition x y = addition (x - 1) (plusOne y)

log2 1 = 0
log2 m = 1 + log2 (quot m 2)

replicate_ :: Int -> a -> [a]
replicate_ 0 x = []
replicate_ m x = x : replicate (m-1) x

findElem :: [a] -> Int -> a
findElem (x:xs) 0 = x
findElem (x:xs) m = findElem xs (m-1)

zip_ :: [a] -> [b] -> [(a,b)]
zip_ [] y = []
zip_ x [] = []
zip_ (x:xs) (y:ys) = (x,y) : zip_ xs ys

length_ :: [a] -> Int
length_ xs = go xs 0
    where go [] res = res
          go (x:xs) res = go xs (res + 1)
