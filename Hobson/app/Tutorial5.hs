module Tutorial5 where 
import Data.List
import Data.Char

--foldr replaces the constructor with sth else
-- defining fold 
fold  :: (a -> b -> b) -> b -> [a] -> b
fold cons empty [] = empty  
fold cons empty (x:xs) = x `cons` (fold cons empty xs)

-- defining foldl 
fold'  :: (b -> a -> b) -> b -> [a] -> b
fold' cons empty [] = empty  
fold' cons empty (x:xs) = fold' cons (cons empty x) xs


data Set a = Empty | Sing a | Union (Set a) (Set a) deriving Show
foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet e s u Empty = e
foldSet e s u (Sing x) =  s x
foldSet e s u (Union x y) =  (foldSet e s u x) `u` (foldSet e s u y)

isIn :: (Eq a) => a -> Set a -> Bool
isIn x = foldSet False (==x) (||)

testSet = Union (Sing 1) (Union (Sing 2) Empty)
muyi = 2 `isIn` testSet

subst :: (Eq a) => Set a -> Set a -> Bool
subst s1 s2 = foldSet True (`isIn` s2) (&&) s1

testSet2 = Union (Sing 1) (Union (Sing 2) (Sing 3))

--instance Eq a => Eq (Set a) wheres
--s == r = (s `subst` r) && (r `subst` s)