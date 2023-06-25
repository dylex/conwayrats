{-# LANGUAGE TupleSections #-}

module Con
  ( Con
  , conVars
  , conGE
  , conBounded
  , conFeasible
  , initCon
  , substituteCon
  ) where

import           Data.List (intercalate, union)
import           Data.Maybe (isJust)
import qualified Linear.Simplex.Simplex as LS
import qualified Linear.Simplex.Types as LS
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.Read as R

import Param
import Sym

data Con = Con
  { _conEQs :: [Sym]
  , _conGEs :: [Sym]
  }

instance Semigroup Con where
  Con e1 g1 <> Con e2 g2 = Con (union e1 e2) (union g1 g2)

instance Monoid Con where
  mempty = Con mempty mempty

conVars :: Con -> Int
conVars (Con e g) = pred $ maximum $ map (length . symCoeffs) (e ++ g)

conEQ, conLT, conLE, conGT, conGE :: Sym -> Sym -> Con
conEQ x y = Con [x - y] []
conLE x y = conGE y x
conGE x y = Con [] [x - y]
conLT x y = conGT y x
conGT x y = conGE x (y + 1)

instance Show Con where
  show (Con eq ge) = intercalate "," $ map (sh "=") eq ++ map (sh ">=") ge where
    sh o s = show (Sym (0:s')) ++ o ++ show (negate c) where
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

conLS :: Con -> [LS.PolyConstraint]
conLS (Con eq ge) =
  map (symcon LS.EQ) eq ++ map (symcon LS.GEQ) ge
  where
  symcon :: (LS.VarConstMap -> Rational -> LS.PolyConstraint) -> Sym -> LS.PolyConstraint
  symcon pc sym = pc (zip [1..toInteger dim] x) (negate c) where
    c:x = map toRational $ symCoeffs sym ++ repeat 0

conFeasibleLS :: Con -> Maybe (LS.DictionaryForm, [Integer], [Integer], Integer)
conFeasibleLS = LS.findFeasibleSolution . conLS

conFeasible :: Con -> Bool
conFeasible = isJust . conFeasibleLS

conBounded :: Con -> Bool
conBounded = all (\(d,s,a,o) -> isJust $ LS.optimizeFeasibleSystem
  (LS.Max (map (, 1) [1..toInteger dim])) d s a o) . conFeasibleLS

initCon :: Con
initCon = Con [] identity

substituteCon :: [Sym] -> Con -> Con
substituteCon sub (Con e g) = Con (map (substitute sub) e) (map (substitute sub) g)
