{-# LANGUAGE TupleSections #-}

module Case
  ( Case(..)
  , loadCases
  , initCase
  , applyCases
  , applyCase
  , lookupCase
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (guard)
import           Data.List (find, intercalate)
import           Data.Maybe (mapMaybe)
import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as RP

import Param
import Sym
import Con

data Case = Case
  { caseCon :: Con
  , caseCounts :: [Sym]
  , caseLabel :: String
  }

instance Show Case where
  show (Case c s l) = show c ++ ": " ++ intercalate ", " (map show s) ++ " [" ++ l ++ "]"

instance Read Case where
  readPrec = Case
    <$> R.readPrec
    <*> (space >> char ':' >> syms)
    <*> (space >> R.lift (RP.option "" (RP.between (RP.char '[') (RP.char ']') (RP.munch (']' /=)))))
    where
    syms = do
      space
      s <- R.readPrec
      (s :) <$> return [] R.+++ (char ',' >> syms)
    char = R.lift . RP.char
    space = R.lift RP.skipSpaces

checkCase :: Case -> Bool
checkCase (Case con counts _) =
  length counts == dim && conVars con <= dim

checkCases :: [Case] -> Maybe (Case, Case)
checkCases [] = Nothing
checkCases (c:l)
  | not (checkCase c) = Just (c, c)
  | otherwise = (c, ) <$> find (conFeasible . (caseCon c <>) . caseCon) l
  <|> checkCases l

loadCases :: FilePath -> IO [Case]
loadCases f = do
  c <- map read . filter filt . lines <$> readFile f
  mapM_ (fail . show) $ checkCases c
  return c
  where
  filt [] = False
  filt ('#':_) = False
  filt _ = True

initCase :: Case
initCase = Case initCon identity ""

substituteCase :: [Sym] -> Case -> Case
substituteCase sub (Case con dig l) =
  Case (substituteCon sub con) (map (substitute sub) dig) l

isNeg :: Con -> Sym -> Bool
isNeg con sym = all (0 >=) (symCoeffs sym) && sym /= 0
  -- || not (conFeasible $ con <> conGE sym 0)

applyCase :: Case -> Case -> Maybe Case
applyCase (Case incon indig l) c = do
  con <- simplifyCon $ incon <> ccon
  guard $ not $ conBounded con
  guard $ not $ isNeg con del
  return $ Case con cdig (l ++ cl)
  where
  Case ccon cdig cl = substituteCase indig c
  scdig = sum cdig
  del = scdig - sum indig

applyCases :: [Case] -> Case -> [Case]
applyCases cs c = mapMaybe (applyCase c) cs

lookupCase :: [Case] -> String -> Maybe Case
lookupCase cs l = find ((l ==) . caseLabel) cs
