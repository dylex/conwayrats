{-# LANGUAGE RecordWildCards #-}

import           Control.Monad (when)
import           Data.Function ((&))
import           Data.List (intercalate)
import           Data.Maybe (isJust)
import qualified System.Console.GetOpt as GetOpt
import           System.Directory (doesFileExist)
import           System.Environment (getProgName, getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, hFlush, stdout, stderr)
import           Text.Read (readMaybe)

import Paths_conwayrats (getDataFileName)
import Param
import Sym
import RLE
import Con
import Case

race :: (Num r, Ord r) => RLE r -> RLE r
race x = carryRLE $ s + flipRLE s where
  s = sortRLE x

showCase :: Opt -> Case -> IO Bool
showCase opt c = do
  putStr $ show c ++ "\t" ++ show s
  if isJust (isConst s)
    then False <$ putStrLn ""
  else if conBounded (caseCon c) then
    False <$ putStrLn " bounded"
  else do
    hFlush stdout
    maybe
      (False <$ putStrLn " infeasible")
      (\(m,x) ->
        True <$ putStrLn (" >= " ++ showFloat m  ++ " (" ++ intercalate "," (map showFloat x) ++ ")"))
      =<< (if optILP opt then conMinIO else return . conMin) (caseCon c)
  where
  s = sum $ caseCounts c

treeCases :: Opt -> [Case] -> Int -> Case -> IO ()
treeCases opt cases depth c
  | optTree opt < depth = return ()
  | otherwise = mapM_ (\c' -> do
      putStr $ replicate depth ' '
      n <- showCase opt c'
      when n $ treeCases opt cases (succ depth) c')
      $ applyCases cases c

showFloat :: Double -> String
showFloat = show . (ceiling :: Double -> Integer)

runCases :: Opt -> [Case] -> [Case] -> Case -> IO ()
runCases opt cases cs c = do
  n <- showCase opt c
  when n $ case cs of
    x:r -> maybe (putStrLn $ caseLabel x ++ " impossible") (runCases opt cases r) $ applyCase c x
    [] -> treeCases opt cases 1 c

data Opt = Opt
  { optILP :: Bool
  , optTree :: Int
  , optDecr :: Bool
  }

defOpt :: Opt
defOpt = Opt
  { optILP = False
  , optTree = 0
  , optDecr = False
  }

optDescrs :: [GetOpt.OptDescr (Opt -> Opt)]
optDescrs =
  [ GetOpt.Option "i" ["ilp"]
    (GetOpt.NoArg (\o -> o{ optILP = True }))
    "Find minimum integral solution to constraints (rather than just simplex)"
  , GetOpt.Option "t" ["tree"]
    (GetOpt.OptArg (\a o -> o{ optTree = maybe 1 read a }) "DEPTH")
    "Explore a tree of possibilites after specified case path"
  , GetOpt.Option "d" ["decrease"]
    (GetOpt.NoArg (\o -> o{ optDecr = True }))
    "Allow exploring cases that decrease count"
  ]

caseFile :: String
caseFile = "case" ++ show base

usage :: IO a
usage = do
  prog <- getProgName
  hPutStrLn stderr $ GetOpt.usageInfo (prog ++ " [OPTIONS] CASEPATH|NUMBER\n\
\Run Conway's RATS, base " ++ show base ++ ".\n\
\If given a simple number, follow it forever.\n\
\If given a list of cases by letter (as specified in the " ++ caseFile ++ " file),\n\
\  evaluate the conditions on the initial digit counts (a=1s,b=2s,...)\n\
\  that would lead to this path of cases, and compute the minimum initial\n\
\  digit counts necessary.\n") optDescrs
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  (opt, arg) <- case GetOpt.getOpt GetOpt.Permute optDescrs args of
    (o, [a], []) -> return (foldl (&) defOpt o, a)
    (_, _, err) -> do
      mapM_ (hPutStrLn stderr) err
      usage
  maybe
    (do
      casefileexists <- doesFileExist caseFile
      cases <- loadCases =<< if casefileexists then return caseFile else getDataFileName caseFile
      maybe usage
        (\cs -> runCases opt (filter ((optDecr opt ||) . not . isNeg . caseDelta) cases) cs initCase)
        $ mapM (lookupCase cases) (map return arg))
    (\x -> mapM_ print $ iterate race (x :: RLE Int))
    $ readMaybe arg
