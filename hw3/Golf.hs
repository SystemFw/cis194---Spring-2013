module Golf where
import Data.List

skips :: [a] -> [[a]]
skips l = map (`takeEveryNth` l) [1..length l] 
  where takeEveryNth n =  map snd . filter ((==) 0  . (`mod` n) . fst) . zip [1..]


localMaxima :: [Integer] -> [Integer]
localMaxima (a:r@(b:c:_))
  | a < b && b > c = [b] ++ localMaxima r
  | otherwise = localMaxima r
localMaxima _ = []


histogram:: [Integer] -> String
histogram xs = unlines $ reverse $ transpose $ map pprint $ zip [0..] count
    where count = map (\n -> length $ filter (== n) xs) [0..9]
          pprint (i, n) = show i ++ "=" ++ replicate n '*'
                          ++ replicate (maximum count - n) ' '
          


