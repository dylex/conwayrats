{-# LANGUAGE RecordWildCards #-}
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
import           Data.List (find, intercalate)
import           Data.Maybe (mapMaybe)
import qualified Text.Read as R
import qualified Text.ParserCombinators.ReadP as RP

import Param
import Sym
import Con

data Case = Case
  { caseCounts :: [Sym]
  , caseCon :: Con
  , caseLabel :: String
  , caseDelta :: Sym
  }

instance Show Case where
  show (Case s c l d) = intercalate ", " (map show s) ++ "\tif " ++ show c ++ "\t[" ++ l ++ "] " ++ show d

instance Read Case where
  readPrec = Case
    <$> syms
    <*> (space >> char 'i' >> char 'f' >> space >> R.readPrec)
    <*> (space >> R.lift (RP.option "" (RP.between (RP.char '[') (RP.char ']') (RP.munch (']' /=)))))
    <*> (space >> R.readPrec)
    where
    syms = do
      space
      s <- R.readPrec
      (s :) <$> return [] R.+++ (char ',' >> syms)
    char = R.lift . RP.char
    space = R.lift RP.skipSpaces

checkCase :: Case -> Bool
checkCase Case{..} =
  length caseCounts == dim && conVars caseCon <= dim

checkCases :: [Case] -> Maybe (Case, Case)
checkCases [] = Nothing
checkCases (c:l)
  | not (checkCase c) = Just (c, c)
  | otherwise = (c, ) <$> find (conFeasible . (caseCon c <>) . caseCon) l
  <|> checkCases l

loadCases :: FilePath -> IO [Case]
loadCases f = do
  c <- map read . filter filt . lines <$> readFile f
  mapM_ (\(a,b) -> fail $ "invalid/overlapping cases:\n" ++ show a ++ "\n" ++ show b) $ checkCases c
  return c
  where
  filt [] = False
  filt ('#':_) = False
  filt _ = True

initCase :: Case
initCase = Case identity initCon "" 0

substituteCase :: [Sym] -> Case -> Case
substituteCase sub c@Case{..} = c
  { caseCounts = map (substitute sub) caseCounts
  , caseCon = substituteCon sub caseCon
  , caseDelta = substitute sub caseDelta
  }

-- isNeg con sym = not (conFeasible $ con <> conGE sym 0)

applyCase :: Case -> Case -> Maybe Case
applyCase (Case indig incon l indel) c = do
  con <- simplifyCon $ incon <> ccon
  -- guard $ not $ conBounded con
  -- guard $ not $ isNeg del
  return $ Case cdig con (l ++ cl) del
  where
  Case cdig ccon cl cdel = substituteCase indig c
  del = indel + cdel

applyCases :: [Case] -> Case -> [Case]
applyCases cs c = mapMaybe (applyCase c) cs

lookupCase :: [Case] -> String -> Maybe Case
lookupCase cs l = find ((l ==) . caseLabel) cs
