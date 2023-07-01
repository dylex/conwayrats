import           Control.Monad (forever, when)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)

import Param
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

showCase :: Case -> String
showCase c = show c ++ " " ++ show (sum $ caseCounts c) ++ ">=" ++ show (conMin (caseCon c))

treeCases :: [Case] -> String -> [Case] -> IO ()
treeCases cases pfx = mapM_ $ \c -> do
  putStrLn $ pfx ++ showCase c
  when (length pfx < 8) $
    treeCases cases (' ':pfx) $ applyCases cases c

runCases :: [Case] -> Case -> IO ()
runCases cs x = do
  putStrLn $ showCase x
  case cs of
    c:r -> mapM_ (runCases r) $ applyCase x c
    _ -> return ()

main :: IO ()
main = do
  args <- getArgs
  cases <- loadCases ("case" ++ show base)
  case args of
    [] -> do
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
    [a] 
      | Just x <- readMaybe a ->
        mapM_ print $ iterate race (x :: RLE Int)
      | Just cs <- mapM (lookupCase cases) $ map return a ->
        runCases (cycle cs) initCase
    _ -> fail "unknown argument"
      {-
      let s = map read args
      forever $ do
        e <- getLine
        print $ substitute s $ read e
      -}
