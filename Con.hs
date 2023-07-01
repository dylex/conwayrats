{-# LANGUAGE TupleSections #-}

module Con
  ( Con
  , conVars
  , conGE
  , conBounded
  , conFeasible
  , conMin
  , initCon
  , substituteCon
  , simplifyCon
  ) where

import           Control.Arrow (first, second)
import           Data.List (intercalate, union)
import           Data.Maybe (isJust)
import qualified Linear.Simplex.Simplex as LS
import qualified Linear.Simplex.Types as LS
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.Read as R

import Param
import Sym

newtype Con = Con
  { _conGEs :: [Sym]
  }

instance Semigroup Con where
  Con g1 <> Con g2 = Con (union g1 g2)

instance Monoid Con where
  mempty = Con mempty

conVars :: Con -> Int
conVars (Con g) = pred $ maximum $ map (length . symCoeffs) g

conEQ, conLT, conLE, conGT, conGE :: Sym -> Sym -> Con
conEQ x y = Con [x - y, y - x]
conLE x y = conGE y x
conGE x y = Con [x - y]
conLT x y = conGT y x
conGT x y = conGE x (y + 1)

instance Show Con where
  show (Con ge) = intercalate "," $ map sh ge where
    sh s = show (Sym (0:s')) ++ ">=" ++ show (negate c) where
      c:s' = symCoeffs s ++ [0]

instance Read Con where
  readPrec = cons where
    cons = do
      x <- R.readPrec
      o <- op
      y <- R.readPrec
      let e = o x y
      (e <>) <$> return mempty R.+++ more
    more = char ',' >> cons
    op = do
      o <- R.get
      case o of
        '=' -> iseq conEQ conEQ
        '>' -> iseq conGT conGE
        '<' -> iseq conLT conLE
        _ -> R.pfail
    iseq st oe = return st R.+++ (oe <$ char '=')
    char = R.lift . RP.char

symToConstr :: Sym -> (LS.VarConstMap, Rational)
symToConstr sym = (zip [1..toInteger dim] x, negate c)
  where c:x = map toRational $ symCoeffs sym ++ repeat 0

conLS :: Con -> [LS.PolyConstraint]
conLS (Con ge) = map symcon ge where
  symcon = uncurry LS.GEQ . symToConstr

conFeasibleLS :: Con -> Maybe (LS.DictionaryForm, [Integer], [Integer], Integer)
conFeasibleLS = LS.findFeasibleSolution . conLS

conFeasible :: Con -> Bool
conFeasible = isJust . conFeasibleLS

conBounded :: Con -> Bool
conBounded = all (\(d,s,a,o) -> isJust $ LS.optimizeFeasibleSystem
  (LS.Max (map (, 1) [1..toInteger dim])) d s a o) . conFeasibleLS

simplex :: LS.ObjectiveFunction -> Con -> Maybe Rational
simplex _ (Con []) = Nothing
simplex obj con = do
  (o, v) <- LS.twoPhaseSimplex obj $ conLS con
  lookup o v

conMin :: Con -> Int
conMin con = maybe (-1) ceiling $
  simplex (LS.Min (map (, 1) [1..toInteger dim])) con

-- |Would adding this single constraint affect the system?
conActive :: Sym -> Con -> Bool
conActive sym con = all (c >) $ simplex (LS.Min m) con where
  (m, c) = symToConstr sym

-- |Initial constraints (all variables non-negative, implicitly)
initCon :: Con
initCon = mempty

substituteCon :: [Sym] -> Con -> Con
substituteCon sub (Con g) = Con (map (substitute sub) g)

simplifyCon :: Con -> Maybe Con
simplifyCon (Con g)
  | any (0 >) gc = Nothing
  | not (conFeasible (Con gs)) = Nothing
  | otherwise = Just $ Con $ simpl [] gs
  where
  (gc, gs) = partc g
  partc [] = ([], [])
  partc (s:r) = maybe (second (s:)) (\c -> first (c:)) (isConst s) $ partc r
  simpl a [] = a
  simpl a (s:r)
    | conActive s (Con (a ++ r)) = simpl (s:a) r
    | otherwise = simpl a r
