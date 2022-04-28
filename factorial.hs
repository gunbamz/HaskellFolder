--solution--
fac n = if n == 0 
           then 1
           else n * fac (n-1)


fac = (\(n) ->
        (if ((==) n 0)
            then 1
            else ((*) n (fac ((-) n 1)))))



fac 0 = 1
fac n = n * fac (n-1)

fac n = foldr (*) 1 [1..n]

fac n = foldl (*) 1 [1..n]

-- using foldr to simulate foldl

fac n = foldr (\x g n -> g (x*n)) id [1..n] 1


facs = scanl (*) 1 [1..]
fac n = facs !! n


fac = foldr (*) 1 . enumFromTo 1


--iterative
ac n = result (for init next done)
        where init = (0,1)
              next   (i,m) = (i+1, m * (i+1))
              done   (i,_) = i==n
              result (_,m) = m

for i n d = until d n i

--iterative one line
fac n = snd (until ((>n) . fst) (\(i,m) -> (i+1, i*m)) (1,1))


facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)
fac = facAcc 1


facCps k 0 = k 1
facCps k n = facCps (k . (n *)) (n-1)
fac = facCps id

y f = f (y f)
fac = y (\f n -> if (n==0) then 1 else n * f (n-1))

s f g x = f x (g x)
k x y   = x
b f g x = f (g x)
c f g x = f x g
y f     = f (y f)
cond p f g x = if p x then f x else g x
fac  = y (b (cond ((==) 0) (k 1)) (b (s (*)) (c b pred)))
