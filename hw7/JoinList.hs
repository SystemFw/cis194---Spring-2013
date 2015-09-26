{-# LANGUAGE FlexibleInstances#-}
module JoinList where
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor
import Data.List

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)


jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Exercise 2

sizeof :: (Sized m, Monoid m) => JoinList m a -> Int
sizeof = getSize . size . tag

-- 2.1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl | i < 0 = Nothing
indexJ  _ Empty = Nothing
indexJ 0  (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ l r)
  | i < (sizeof l) = indexJ i l
  | otherwise = indexJ (i - sizeof l) r

(!!?) :: [a] -> Int -> Maybe a
[]  !!? _  = Nothing
_   !!? i | i < 0 = Nothing 
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

prop_index   :: (Eq a, Monoid b, Sized b) => Int -> JoinList b a -> Bool
prop_index i jl  =  (indexJ i jl) == (jlToList jl !!? i)

-- 2.2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl |  i <= 0 = jl
dropJ _ Empty = Empty
dropJ _ (Single _ _) =  Empty 
dropJ i (Append _ l r)
  | i < sizeof l = (dropJ i l) +++ r
  | otherwise = Empty +++ dropJ (i - sizeof l) r

prop_drop   :: (Eq a, Monoid b, Sized b) => Int -> JoinList b a -> Bool
prop_drop n jl =  jlToList (dropJ n jl) == drop n (jlToList jl)

-- 2.3

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl | i <= 0 = Empty
takeJ _ Empty = Empty
takeJ _ el@(Single _ _) = el
takeJ i (Append _ l r) 
  | i <= sizeof l = takeJ i l
  | otherwise = l +++ takeJ (i - sizeof l) r
  
prop_take   :: (Eq a, Monoid b, Sized b) => Int -> JoinList b a -> Bool
prop_take n jl = jlToList (takeJ n jl) == take n (jlToList jl)


  

testData = Append (Size 4)(Append (Size 3)
     (Single (Size 1) 'y')
     (Append (Size 2)
       (Single (Size 1) 'e')
       (Single (Size 1) 'a')))
   (Single (Size 1) 'h')

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine = Single <$> scoreString <*> id

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString = concat . jlToList
  fromString = foldl1' (+++) . map (\s -> Single (scoreString s, 1) s) . lines
  numLines = sizeof                 
  value = getScore . fst . tag
  line = indexJ
  replaceLine lineNum newStr  buf
   | lineNum < 0 ||lineNum > numLines buf  = buf
   | otherwise  = let a = takeJ lineNum buf                                       
                      b = dropJ (lineNum+1) buf                         
                  in a +++ fromString newStr +++ b



main = runEditor editor $ scratch
  where scratch :: JoinList (Score, Size) String
        scratch = fromString $ unlines
                  [ "This buffer is for notes you don't want to save, and for"
                  , "evaluation of steam valve coefficients."
                  , "To load a different file, type the character L followed"
                  , "by the name of the file."
                  ]


       
