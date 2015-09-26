{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk  where
--Exercise 1
import Control.Monad.Random
import Control.Monad
import Control.Arrow (first)
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
                 deriving Show


-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield a d)
  | a <= 1 || d <= 0 = pure b
  | otherwise = updateField b <$> (countCasualties <$> dieRoll nOfAs <*> dieRoll nOfDs)
  where nOfAs = min (a - 1) 3                  
        nOfDs = min d 2            

                    
dieRoll :: Int -> Rand StdGen [DieValue]
dieRoll n = (reverse . sort) <$> replicateM n die

countCasualties :: [DieValue] -> [DieValue] -> (Army,Army)
countCasualties as ds =  foldr (sumPairs) (0,0) $ zipWith count as ds
  where count a d
          | a > d = (0,1)
          | otherwise = (1,0)
        sumPairs (a,b) (a',b') = (a + a',b + b')

updateField :: Battlefield -> (Army,Army) -> Battlefield
updateField b (a,d) = b {attackers = attackers b - a , defenders = defenders b - d}

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b =  do
  field <- battle b
  if attackers b < 2 || defenders b == 0
    then return field
    else invade field

-- Exercise 4

attempts :: Integer
attempts = 10000

successProb :: Battlefield -> Rand StdGen Double
successProb b = computeProb <$> (replicateM n $ invade b)
  where n = fromInteger attempts
        computeProb = (/ m) . fromIntegral . length . filter success
        success = (== 0) . defenders
        m = fromInteger attempts

main :: IO ()
main = (evalRandIO $ successProb $ Battlefield 5 4) >>= print
