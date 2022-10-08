module Tutorial4 where

data Q = Q Integer Integer

instance Show Q where 
    show (Q n d) = concat [show n, "/", show d]

simpQ :: Q -> Q
simpQ (Q n d) = Q (n `div` c) (d `div` c)
   where c = gcd n d

instance Eq Q where
    r1 == r2 = (n1 == n2) && (d1 == d2)
        where (Q n1 d1) simpQ r1
              (Q n2 d2) simpQ r2

addQ :: Q -> Q -> Q
addQ(Q n1 d1) (Q n2 d2 = simpQ $ Q (n1' + n2') m
   where m = lcm d1 d2
   n1' = n1 * (m `div` d1)
   n2' = n2 * (m `div` d2)

multQ (Q n1 d1) (Q n2 d2) = simQ $ Q (n1 * n2) (d1 * d2)

instance Num Q where
     (+) = addQ
     negate (Q n d) = Q (-n) d
     (*) = multQ
     abs (Q n d) = Q (abs n) (abs d)
     signum (Q n d) Q (signum n * signum d) 1
     fromInteger n = Q n 1

 newtype RevString = RevString String

 instance Show RevString where
     show (RevString s) = reverse s

s = "Hello world"
s' = RevString s