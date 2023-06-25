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
  ) where

import           Data.List (intercalate, union)
import           Data.Maybe (isJust, fromMaybe)
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

conLS :: Con -> [LS.PolyConstraint]
conLS (Con ge) = map symcon ge where
  symcon sym = LS.GEQ (zip [1..toInteger dim] x) (negate c) where
    c:x = map toRational $ symCoeffs sym ++ repeat 0

conFeasibleLS :: Con -> Maybe (LS.DictionaryForm, [Integer], [Integer], Integer)
conFeasibleLS = LS.findFeasibleSolution . conLS

conFeasible :: Con -> Bool
conFeasible = isJust . conFeasibleLS

conBounded :: Con -> Bool
conBounded = all (\(d,s,a,o) -> isJust $ LS.optimizeFeasibleSystem
  (LS.Max (map (, 1) [1..toInteger dim])) d s a o) . conFeasibleLS

conMin :: Con -> Int
conMin con = fromMaybe (-1) $ do
  (o, v) <- LS.twoPhaseSimplex (LS.Min (map (, 1) [1..toInteger dim])) $ conLS con
  ceiling <$> lookup o v

initCon :: Con
initCon = Con identity

substituteCon :: [Sym] -> Con -> Con
substituteCon sub (Con g) = Con (map (substitute sub) g)
