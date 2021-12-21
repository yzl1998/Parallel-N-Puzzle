module Main where
import GHC.Conc(par)
import Control.Parallel.Strategies hiding(parMap)
import Control.DeepSeq
import Data.List.Split
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Grid (getSolvedGrid)
import Solver (solve, solve')
import Logger
import Parser

checkArgs :: [String] -> IO [String]
checkArgs xs = if null xs then displayHelp >> exitSuccess else pure xs

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do b <- rpar (f a)
                     bs <- parMap f as
                     return (b:bs)

main :: IO ()
main = do
    args <- checkArgs =<< getArgs
    content <- readFile $ args !! 0
    let input = splitOn "#" content
        inputs = map (\x -> (words <$> (drop 1 . clearInput . lines $ x))) input
        grids = map transformInput1 inputs
        (as, bs) = splitAt (length grids `div` 2) grids
        solution =  map (\x -> solve' (getSolvedGrid $ length x) x (parseArgs args)) grids
        {- runEval (parMap (\x -> solve' (getSolvedGrid $ length x) x (parseArgs args)) grids) -}
        {-           runEval $ do
                            as' <- rpar (force (map (\x -> solve' (getSolvedGrid $ length x) x (parseArgs args)) as))
                            bs' <- rpar (force (map (\x -> solve' (getSolvedGrid $ length x) x (parseArgs args)) bs))
                            rseq as'
                            rseq bs'
                            return (as'++ bs') -}
    print (length solution) >> mapM_ print solution
