import Data.List 

type Grid             = Matrix Value
type Matrix a         = [Row a]
type Row a            = [a]
type Value            = Char
chop                  :: Int -> [a] -> [[a]]
chop n []             =  []
chop n xs             =  take n xs : chop n (drop n xs)
rows                  :: Matrix a -> [Row a]
rows                  =  id
cols                  :: Matrix a -> [Row a]
cols                  =  transpose

boxsize               :: Int
boxsize               =  2
values                :: [Value]
values                =  ['1'..'9']
empty                 :: Value -> Bool
empty                 =  (== '.')
single                :: [a] -> Bool
single [_]            =  True
single _              =  False

boxs :: Matrix a -> [Row a]
boxs =  unpack . map cols . pack
                          where
                             pack   = split . map split
                             split  = chop boxsize
                             unpack = map concat . concat

> expand                :: Matrix Int -> [Matrix Int]
> expand m              =
>    [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
>    where
>       (rows1,row:rows2) = break (any (not . single)) m
>       (row1,cs:row2)    = break (not . single) row

