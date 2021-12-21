module Main where
import GHC.Conc(par)
import Control.Parallel.Strategies
import Data.List.Split
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Grid (getSolvedGrid)
import Solver (solve, solve')
import Logger
import Parser

checkArgs :: [String] -> IO [String]
checkArgs xs = if null xs then displayHelp >> exitSuccess else pure xs


main :: IO ()
main = do
    args <- checkArgs =<< getArgs
    content <- readFile $ args !! 0
    let input = splitOn "#" content
        inputs = map (\x -> (words <$> (drop 1 . clearInput . lines $ x))) input
        grids = map transformInput1 inputs
        (as, bs) = splitAt (length grids `div` 2) grids
        solution = runEval $ do
                            as' <- rpar (map (\x -> solve' (getSolvedGrid $ length x) x (parseArgs args)) as)
                            bs' <- rpar (map (\x -> solve' (getSolvedGrid $ length x) x (parseArgs args)) bs)
                            rseq as'
                            rseq bs'
                            return (as'++ bs')
    print (length solution)
