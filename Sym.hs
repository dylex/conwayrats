module Sym
  ( Sym(..)
  , identity
  , substitute
  ) where

import           Data.Maybe (maybeToList)
import qualified Text.Read as R

import Param

newtype Sym = Sym{ symCoeffs :: [Int] }

(*:) :: Int -> Int -> Sym
x *: v = Sym $ replicate v 0 ++ [x]

zipWithZ :: a -> (a -> a -> b) -> [a] -> [a] -> [b]
zipWithZ z f (x:xs) (y:ys) = f x y : zipWithZ z f xs ys
zipWithZ z f [] (y:ys) = f z y : zipWithZ z f [] ys
zipWithZ z f (x:xs) [] = f x z : zipWithZ z f xs []
zipWithZ _ _ [] [] = []

instance Eq Sym where
  Sym x == Sym y = and $ zipWithZ 0 (==) x y

instance Num Sym where
  fromInteger = Sym . return . fromInteger
  Sym x + Sym y = Sym (zipWithZ 0 (+) x y)
  Sym x - Sym y = Sym (zipWithZ 0 (-) x y)
  negate (Sym x) = Sym (map negate x)
  abs (Sym x) = Sym (map abs x)
  signum (Sym x) = Sym (map signum x)
  Sym [] * _ = Sym []
  Sym [c] * Sym x = Sym (map (c *) x)
  Sym x * Sym [c] = Sym (map (* c) x)
  _ * _ = error "Sym.(*) not supported"

var1 :: Char
var1 = 'a'

instance Show Sym where
  showsPrec p (Sym v0) = showParen (p > 6 && not (triv v0)) $ st True Nothing v0 where
    st True _ [] = ('0':)
    st False _ [] = id
    st i l (0:v) = st i (nv l) v
    st False l v@(n:_) | n > 0 = ('+' :) . st True l v
    st _ (Just l) (1:v) = (l :) . st False (Just $ succ l) v
    st _ (Just l) (-1:v) = ('-' :) . (l :) . st False (Just $ succ l) v
    st _ l (n:v) = shows n . (maybeToList l ++) . st False (nv l) v
    nv Nothing = Just var1
    nv (Just v) = Just (succ v)
    triv (0:l) = triv' l
    triv _ = False
    triv' (0:l) = triv' l
    triv' (1:l) = all (0 ==) l
    triv' _ = False

instance Read Sym where
  readPrec = expr where
    expr = do
      t <- term
      return t R.+++ R.prec 6 (oper t)
    oper t = do
      o <- op
      e <- term
      let t' = t `o` e
      return t' R.+++ oper t'
    op = do
      o <- R.get
      case o of
        '+' -> return (+)
        '-' -> return (-)
        _ -> R.pfail
    term = R.prec 9 $ do
      n <- R.readPrec R.<++ return 1
      v <- var R.<++ return 0
      return $ n *: v
    var = do
      c <- R.get
      let d = fromEnum c - fromEnum var1
      if d >= 0 && d < dim then return $ succ d else R.pfail

identity :: [Sym]
identity = map (1 *:) [1..dim]

substitute :: [Sym] -> Sym -> Sym
substitute sub (Sym (c:v)) = sum $ Sym [c] : zipWith (\s x -> Sym [x] * s) sub v
substitute _ (Sym []) = Sym []
