{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module RLE
  ( RLE
  , countRLE
  , carryRLE
  , sortRLE
  , flipRLE
  ) where

import           Data.Char (isDigit, digitToInt, intToDigit)
import           Data.List (sortOn)
import           Data.Ord (Down(Down))
import qualified Text.Read as R

import Param
import Sym

type Digit = Int

data Run r = Run
  { runLength :: r
  , runDigit :: Digit
  }

newtype RLE r = RLE{ unRLE :: [Run r] }

onRLE :: ([Run a] -> [Run b]) -> RLE a -> RLE b
onRLE f = RLE . f . unRLE

zipRLEWith :: (Ord r, Num r) => (Digit -> Digit -> Digit) -> RLE r -> RLE r -> RLE r
zipRLEWith f (RLE rx) (RLE ry) = RLE $ zrle rx ry where
  zrle [] [] = []
  zrle (Run a x:r) [] = Run a (f x 0) : zrle r []
  zrle [] (Run a y:r) = Run a (f 0 y) : zrle [] r
  zrle (Run a x:r) (Run b y:s) = case compare a b of
    LT -> Run a (f x y) : zrle r (Run (b-a) y:s)
    EQ -> Run a (f x y) : zrle r s
    GT -> Run b (f x y) : zrle (Run (a-b) x:r) s

instance (Ord r, Num r) => Num (RLE r) where
  fromInteger = RLE . return . Run 1 . fromInteger
  (+) = zipRLEWith (+)
  (-) = zipRLEWith (-)
  (*) = error "RLE.(*) not supported"

instance Show (Run Sym) where
  showsPrec p (Run s d) = showsPrec 10 s . showsPrec p d  

instance Show (Run Int) where
  show (Run l d) = replicate l (intToDigit d)

instance Show (Run r) => Show (RLE r) where
  show = sl . reverse . unRLE where
    sl [] = ""
    sl (r:l) = show r ++ sl l

readDigit :: R.ReadPrec Digit
readDigit = do
  c <- R.get
  if isDigit c then return $ digitToInt c else R.pfail

instance Read (Run Sym) where
  readPrec = do
    s <- R.parens R.readPrec
    d <- readDigit
    return $ Run s d

instance Read (Run Int) where
  readPrec = do
    d <- readDigit
    return $ Run 1 d

instance Read (Run r) => Read (RLE r) where
  readPrec = RLE . reverse <$> rl where
    rl = do
      r <- R.readPrec
      (r :) <$> return [] R.+++ rl

compactRLE :: (Eq r, Num r) => RLE r -> RLE r
compactRLE = onRLE comp where
  comp [] = []
  comp [Run _ 0] = []
  comp (Run a _:l)
    | a == 0 = comp l
  comp (Run a x:Run b y:l)
    | x == y = comp (Run (a+b) x :l)
  comp (r:l) = r : comp l

carryRLE :: Num r => RLE r -> RLE r
carryRLE = onRLE (crle 0) where
  crle 0 [] = []
  crle c [] = [Run 1 c]
  crle c (Run n d:l)
    | c1' /= c' = error "carryRLE: overflow"
    | otherwise =
      Run 1 d1' : Run (n-1) d' : crle c' l
    where
    (c1', d1') = divMod (d+c) base
    (c', d') = divMod (d+c1') base

flipRLE :: RLE r -> RLE r
flipRLE = onRLE reverse

sortRLE :: (Eq r, Num r) => RLE r -> RLE r
sortRLE = compactRLE . onRLE (sortOn $ Down . runDigit)

countRLE :: Num r => RLE r -> r
countRLE = sum . map runLength . unRLE
