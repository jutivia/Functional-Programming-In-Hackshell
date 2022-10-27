doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

doubleSmall x = if x > 100
                then x
                else doubleMe x

mult :: [Int] -> [String]
mult s = [if q `mod` 2 == 0 then "two" else "no" | q<-s]

rightTriangle :: [(Int, Int, Int)]
rightTriangle = [(a,b,c) | c<- [1..10], b<- [1..c], a<-[1..b], a^2 + b^2 == c^2, a+b+c == 24 ]
n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

f ns = ns !! ((length ns) -1) 
reverseF ns = head(reverse ns)

initF ns = take (length ns -1) ns

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b-> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrone :: Eq a => [a] -> Bool
palindrone xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

safeTail xs = if null xs then 
                [] 
                else 
                tail xs
safeTailGuards xs | null xs = []
                  | otherwise = tail xs

safeTailPatternMatching [] = []
safeTailPatternMatching (_:xs) = xs


True || True = True
_ || _ = True

checkTrue :: Bool -> Bool -> Bool
checkTrue a b | a== True && a == b  =True
              | otherwise = False

checkTrueTwo :: Bool -> Bool -> Bool
checkTrueTwo a b |  a == True = b
                 |  a == False = False 
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

checkPrime :: Int -> Bool
checkPrime n | factors n == [1, n] = True
             | otherwise = False 

primes :: Int -> [Int]
primes n = [x | x <- [2..n], checkPrime x]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x' ]

count :: Eq a=> a-> [a] -> Int 
count x xs = sum[ 1 | x' <- xs, x == x' ]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(z,y,x)| x <- [1..n], y <- [1..x], z <- [1..y], y^2 + z^2 == x^2]

perefcts :: Int -> [Int]
perefcts n = [x| x<- [1..n], sum(init(factors x)) == x ]
scalarProd :: [Int] -> [Int] -> Int
scalarProd xs ys= sum[x' * y' | (x',y') <- zip xs ys]

sp  xs ys = sum[xs!! i * ys !! i | i <- [0..n-1]]
            where n = length xs

fac 0 = 1
fac n = n * fac(n-1)

products [] = 1
products (n: ns) = n * products ns

band :: [Bool] -> Bool
band [] = True
band (b: bs) = b && (band bs)
conc [] = []
conc (xs : xss) = xs ++ conc xss

repl 0 _ = []
repl n x = x : repl (n-1) x

index (x:_) 0 = x
index [] n = -1
index (_:xs) n  = index xs (n-1)

findIndex :: Int -> [Int] -> Int
findIndex x xs = sum[1 | x' <- xs, x' < x] 
insert :: Int -> [Int]-> [Int]
insert x xs = take (findIndex x xs) xs ++ [x] ++ drop (findIndex x xs) xs

recursionInsert :: Int -> [Int] -> [Int]
recursionInsert x [] = [x]
recursionInsert x (y: ys) | x <= y = x: y: ys 
                          | otherwise = y : recursionInsert x (ys)

insertionSort [] = []
insertionSort (y: ys) = recursionInsert y (insertionSort ys)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x: xs) (y:ys) | x <= y =  x: merge xs (y:ys)
                     | y <= x = y: merge ys (x:xs)

firstHalf xs = [ys | ys <- take ((length xs )`div` 2) xs] 
secondHalf xs = [zs | zs <- drop ((length xs) `div` 2) xs] 

half xs = (firstHalf xs, secondHalf xs)

msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)   
             where (ys, zs) = half xs





sums xs = foldr (+) 0 xs
prods xs = foldr (*) 1 xs

drops :: Int -> [a] -> [a]
drops 0 xs = xs
drops _ [] = []
drops n (_:xs) = drop (n-1) xs


initial :: [a] -> [a]
initial [_] = []
initial (x:xs) = x : initial xs 

type Pos = (Int, Int)
origin :: Pos
origin = (0,0)
left :: Pos -> Pos
left (x,y) = (x-1, y-1) 

type Pair a = (a,a)
multiply :: Pair Int -> Int
multiply (x,y) = x*y

copy :: a -> Pair a
copy x = (x,x)

type Trans = Pos -> Pos
transform :: Trans 
transform (a,b) = (b,a)

data Answer = Yes | No | Unknown
answers = [Yes , No , Unknown]

flip :: Answer -> Answer
flip x | x == Yes = No
       | x == No = Yes
       | otherwise = Unknown

data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square n = React n n


