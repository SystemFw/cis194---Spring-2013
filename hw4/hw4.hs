import  Data.List 

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even
                   
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- I think the original is better fun2 though, so poor exercise
fun2' :: Integer -> Integer
fun2' = sum .
        filter even .
        takeWhile (> 1) .
        iterate (\n -> if even n then n `div` 2 else 3 * n + 1 ) .
        \n -> if even n then n*2 else n


data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 1
height (Node _ l _ r) = 1 + max (height r) (height l)

-- generates a balanced binary tree
foldTree :: [a] -> Tree a
foldTree l = let h = floor $ logBase 2 $ fromInteger $ genericLength l
                 in foldr (treeInsert h) Leaf l

treeInsert :: Integer -> a -> Tree a -> Tree a
treeInsert level a Leaf = Node level Leaf a Leaf
treeInsert level a (Node h left v right)
  |height left <= height right = Node h (treeInsert (level - 1) a left) v right
  |otherwise = Node h left v (treeInsert (level - 1) a right)


xor :: [Bool] -> Bool   
xor = foldr (/=) False

xor' :: [Bool] -> Bool
xor' = odd . length . filter (== True)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []


--myFoldl :: (a -> b -> a) -> a -> [b] -> a
--myFoldl f z xs = foldr (\f g 

--foldl f z (x:xs) = foldl f (f z  x) xs

--foldr f z (x:xs) = f x (foldr f z xs)

rev :: [a] ->[a]
rev = foldl (flip (:)) []

sundaramSieve :: Integer -> [Integer]
sundaramSieve n = map (\x-> 2*x+1) $ [1..n] \\ nonprimes 
  where upper = (n-1) `div` 2
        nonprimes = [i+j+2*i*j | j <- [1..upper], i <- [1..j]]

