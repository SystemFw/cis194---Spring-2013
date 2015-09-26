module Party where
import Employee
import Data.Tree
import Control.Arrow
import System.Environment (getArgs)

-- Exercise 1

-- 1.1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL emps fun) = GL (e:emps) $ fun + empFun e

-- 1.2

instance Monoid GuestList where
  mempty = GL [] 0
  (GL e1 f1) `mappend` (GL e2 f2) = GL (e1 ++ e2) $ f1 + f2

-- 1.3

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b)  -> Tree a -> b
treeFold f (Node root children) = f root $ map (treeFold f) children


-- Exercise 3

nextLevel :: Employee ->
             [(GuestList,GuestList)] -> -- best with sub-boss, best without sub-boss
             (GuestList,GuestList)  -- best with boss, best without boss
nextLevel boss =  first (glCons boss). mconcat . map (snd &&& uncurry moreFun)

-- Exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


-- Exercise 5

pprint :: GuestList -> String
pprint (GL e f) = "Total fun: " ++ show f ++ "\nGuests: \n" ++ (unlines $  map render e)
  where render (Emp name fun) = "Name: " ++ name ++ " Fun: " ++ show fun

main :: IO ()
main = getArgs >>=
       readFile . head >>=
       putStrLn . pprint . maxFun . read
