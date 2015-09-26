{-# LANGUAGE FlexibleInstances #-}
module Calc where

import qualified ExprT as E
import Parser
import qualified StackVM as S
import qualified Data.Map as M
import Control.Applicative

-- Exercise 1

eval :: E.ExprT -> Integer
eval (E.Lit n) = n
eval (E.Add l r) = eval l + eval r
eval (E.Mul l r) = eval l * eval r

-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a
    | a > 0 = True
    | otherwise = False  -- =  if a <= 0 then False else True
  add = (||)
  mul = (&&)

newtype MinMax  = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax . id
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where 
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 . (`mod` 7) $ a + b
  mul (Mod7 a) (Mod7 b) = Mod7 . (`mod` 7) $ a * b

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-- Exercise 5

instance Expr S.Program  where
  lit a = [S.PushI a]
  add a b = a ++ b ++ [S.Add]
  mul a b = a ++ b ++ [S.Mul]

compile :: String -> Maybe S.Program
compile =  parseExp lit add mul

runVM :: String -> Either String S.StackVal
runVM = maybe (Left $ "ParseError") id . fmap S.stackVM . compile

-- Exercise 6

class HasVars a where
   var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup 

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = const $ Just  a
  add a b = liftA2 (+) <$> a <*> b
  mul a b = liftA2 (*) <$> a <*> b

withVars :: [(String, Integer)] ->
            (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
