module CreditCardValidation (validate) where 

toDigits :: Integer -> [Integer]
toDigits = reverse . map (`mod` 10) . takeWhile (>0) . iterate (`div` 10) 

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (*) oneTwo 
  where oneTwo = 1:2:oneTwo

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate =  (==) 0 . (`mod` 10) . sumDigits . doubleEveryOther. reverse . toDigits
