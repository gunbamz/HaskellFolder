--x =  sum [1..100]
--[1,2,3,4] ++ [6,7,8]
--1 : [4,5,6]
--f [] = []
--x = 1 : [2,4]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
               where
                   ys = [ a | a <- xs, a <= x]
                   zs = [ b | b <- xs, b > x]

w = reverse [5,7,2,9,7,1,4]
tee = [x | x <- [1..10], even x]

factors :: Int -> [Int]
factors n = [x | x <- [1..10], n `mod` x == 0]

prime :: Int -> Bool
prime m = factors n == [1,n]

primes :: Int -> [Int]
primes p = [x | x <- [2..n], prime x]

--sieve if erythorpteny
allprimes :: [Int]
allprimes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [s | x <- xs, x `mod` p /= 0]

-- gbam = takewhile (<= 15000) all prime


fac :: Int -> Int
fac n = product [1..n]

--recursion
fac 0 = 1
fac n = n * fac (n-1)

-- product recursion
product :: Num a => [a] -> a
product [] = []
product (n:ns) = n * product ns
ghci
--higher order function
map :: (a -> b) -> [a] -> [b]
map f xs =[f x | x <- xs]

--higher order function recursion
map f [] = []
map f (x: xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [b]
filter even [1..10]
filter p xs = [x | x x <- xs, p x]


filter p [] = []
filter p (x:xs)
     | p x = x : filter p xs
     | otherwise = filter p xs






