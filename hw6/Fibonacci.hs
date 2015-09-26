{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

--exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

--exercise 3

-- I could define instances for at least Functor and Applicative (ZipList style)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a as) =  a : streamToList as

instance Show a => Show (Stream a) where
     show = show . take n . streamToList

n = 100

--exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) $ streamMap f as

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f $ f seed 

--exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

{-
The ruler stream corresponds to the ruler function 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,..
where the nth element in the stream (assuming the first element corresponds to n = 1) is the largest power of 2 which evenly divides n.
-}

rulerFunction :: Integer -> Integer
rulerFunction n = maximum [p | p <- [0..n], n `mod` 2^p == 0]

ruler :: Stream Integer
ruler = values 0
  where values n = interleaveStreams (streamRepeat n) (values (n+1))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a as) ~(Cons b bs) = Cons a $ Cons b $ interleaveStreams as bs
{-
I have to use (~)to make the second argument explicitly lazy!!
Since Cons forces the evaluation of the second list, but in ruler the second list is produced by a non-terminating function, so I want to suspend its computation .
An alternative definition could be:
 interleaveStreams (Cons a as) bs = Cons a $ interleaveStreams bs as
Who would have thought that sometimes you need even more laziness!
Lesson: PATTERN MATCHING DRIVES EVALUATION.
-}

--exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Cons a as) (Cons b bs) = Cons (f a b) $ streamZipWith f as bs

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate = streamMap negate
  (+) = streamZipWith (+)
  (*) (Cons a as) s@(Cons b bs)= Cons (a*b) $ (streamMap (a*) bs) + as*s

instance Fractional (Stream Integer) where
  (/) (Cons a as)(Cons b bs) =  q
    where q = Cons (a `div` b) $ streamMap (`div` b)(as - q*bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2) -- wow

--exercise 7

data Matrix = Matrix {topLeft::Integer,topRight::Integer,bottomLeft::Integer,bottomRight::Integer}

instance Show Matrix where
  show a = show (topLeft a) ++" "++ show (topRight a) ++ "\n" ++ show (bottomLeft a) ++" "++ show (bottomRight a)


instance Num Matrix where
  (*) a b = let tl = topLeft a * topLeft b + topRight a * bottomLeft b
                bl = bottomLeft a * topRight b + bottomRight a + bottomRight b
                tr = topLeft a * topRight b + topRight a * bottomRight b
                br = bottomLeft a * topRight b + bottomRight a * bottomRight b
            in Matrix { topLeft = tl, topRight = tr, bottomLeft = bl, bottomRight = br}

fibMatrix :: Matrix
fibMatrix = Matrix {topLeft = 1,topRight = 1,bottomLeft = 1, bottomRight = 0}

fib4 n = topRight $ fibMatrix^n
