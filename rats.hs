import           Control.Monad (forever, when)
import           System.Environment (getArgs)

import Sym
import RLE
import Con
import Case

rleCase :: RLE Sym -> (Sym, RLE Sym, Sym)
rleCase x = (n, y, countRLE y - n)
  where
  n = countRLE x
  y = sortRLE $ carryRLE x

race :: (Num r, Ord r) => RLE r -> RLE r
race x = carryRLE $ s + flipRLE s where
  s = sortRLE x

firstCase :: [Case] -> Case -> IO ()
firstCase cases c = do
  print c
  let res = applyCases cases c
  case res of
    [] -> return ()
    (c':_) -> firstCase cases c'

allCases :: [Case] -> [Case] -> IO ()
allCases cases cs = do
  mapM_ print cs
  putStrLn ""
  allCases cases $ foldMap (applyCases cases) cs

treeCases :: [Case] -> String -> [Case] -> IO ()
treeCases cases pfx = mapM_ $ \c -> do
  putStrLn $ pfx ++ show (c) ++ " " ++ show (sum $ caseCounts c) ++ ">=" ++ show (conMin (caseCon c))
  when (length pfx < 8) $
    treeCases cases (' ':pfx) $ applyCases cases c

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      cases <- loadCases "case4"
      -- firstCase cases initCase
      treeCases cases "" [initCase]
      {- forever $ do
      l <- getLine 
      {-
      let c = read l :: Con
      print c
      print $ conBounded dim c
      -}
      {- print $ rleCase $ read l -}
      print $ (read l :: Case) -}
    [a] -> do
      mapM_ print $ iterate race (read a :: RLE Int)
      -- mapM_ print cases
    subs -> do
      let s = map read subs
      forever $ do
        e <- getLine
        print $ substitute s $ read e
