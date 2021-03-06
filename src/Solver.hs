module Solver (SearchType(..), readSearchType, solve, solve') where
    import qualified Data.PQueue.Prio.Min as PQ
    import qualified Data.HashSet as S
    import Logger
    import Grid
    import Distance

    data SearchType = Astar | Uniform | Greedy deriving Eq
    type NextNodesFunc = Int -> [Grid] -> PQ.MinPQueue Int [Grid]

    instance Show SearchType where
        show Astar      = "A*"
        show Uniform    = "Uniform cost"
        show Greedy     = "Greedy"

    readSearchType :: String -> Maybe SearchType
    readSearchType s = case s of
        "astar"     -> Just Astar
        "uniform"   -> Just Uniform
        "greedy"    -> Just Greedy
        _           -> Nothing

    -- Returns the cost of a node according to the SearchType currently used
    getCost :: SearchType -> Distance -> Int -> Grid -> Grid -> Int
    getCost st d l grid goal = let dist = calcDistance d grid goal in case st of
        Astar     -> dist + l  -- A* : h cost + g cost
        Uniform   -> l         -- Uniform : g cost only
        Greedy    -> dist      -- Greedy : h cost only

    -- Returns a PQueue containing the next nodes (value + cost)
    getNextNodes :: Grid -> Distance -> SearchType -> Int -> [Grid] -> PQ.MinPQueue Int [Grid]
    getNextNodes goal d st l xss = PQ.fromList $ zip costs neighbors where
        costs       = (getCost st d l goal) <$> head <$> neighbors
        neighbors   = map (:xss) $ getNeighbors $ head xss

    -- Runs the search using a given SearchType. The SearchType will be used in nodes cost computation
    -- goal : stage to reach ; xss : path from begining to current node ; os : open set ; cs : close set ; nn : nextNodes function ; n : time complexity ; m : space complexity ; l : xss length
    runSearch :: Grid -> [Grid] -> PQ.MinPQueue Int [Grid] -> S.Set Grid -> NextNodesFunc -> Int -> Int -> Int -> IO ()
    runSearch goal xss os cs nn n m l
        | head xss == goal                          = mapM_ displayGrid (reverse xss) >> putStrLn ("Solved with :\n- Time complexity  : " ++ show n ++ "\n- Space complexity : " ++ show m)
        | suc /= PQ.empty                           = runSearch  goal  ((minim suc):xss)  (PQ.union os $ PQ.deleteMin suc)  cs'  nn  (n+1)  size (l+1)
        | suc == PQ.empty && os /= PQ.empty         = runSearch  goal  (tail xss)         os                                cs'  nn  (n+1)  size (l-1)
        | otherwise = putErr NotSolvable where
            suc     = PQ.filter (\x -> S.notMember (head x) cs) $ nn l xss
            cs'     = S.insert (head xss) cs
            minim x = head . snd $ PQ.findMin x
            size    = if PQ.size os > m then PQ.size os else m

    -- Runs the search using a given SearchType. The SearchType will be used in nodes cost computation
    -- goal : stage to reach ; xss : path from begining to current node ; os : open set ; cs : close set ; nn : nextNodes function ; n : time complexity ; m : space complexity ; l : xss length
    runSearch' :: Grid -> [Grid] -> PQ.MinPQueue Int [Grid] -> S.Set Grid -> NextNodesFunc -> Int -> Int -> Int -> Int
    runSearch' goal xss os cs nn n m l
        | head xss == goal                          = n
        | suc /= PQ.empty                           = runSearch'  goal  ((minim suc):xss)  (PQ.union os $ PQ.deleteMin suc)  cs'  nn  (n+1)  size (l+1)
        | suc == PQ.empty && os /= PQ.empty         = runSearch'  goal  (tail xss)         os                                cs'  nn  (n+1)  size (l-1)
        | otherwise = 0 where
            suc     = PQ.filter (\x -> S.notMember (head x) cs) $ nn l xss
            cs'     = S.insert (head xss) cs
            minim x = head . snd $ PQ.findMin x
            size    = if PQ.size os > m then PQ.size os else m

    defaultSearch :: SearchType
    defaultSearch = Astar

    defaultHeuristic :: Distance
    defaultHeuristic = Manhattan

    solve :: Grid -> Grid -> (Maybe SearchType, Maybe Distance) -> IO ()
    solve goal xs (Nothing, Nothing) = putStrLn ( "Solving grid using the " ++ show defaultSearch ++ " algorithm and the " ++ show defaultHeuristic ++ " distance" )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal defaultHeuristic defaultSearch )  0 0 1
    solve goal xs (Just st, Nothing) = putStrLn ( "Solving grid using the " ++ show st            ++ " algorithm and the " ++ show defaultHeuristic ++ " distance" )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal defaultHeuristic st            )  0 0 1
    solve goal xs (Nothing, Just d)  = putStrLn ( "Solving grid using the " ++ show defaultSearch ++ " algorithm and the " ++ show d                ++ " distance" )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal d                defaultSearch )  0 0 1
    solve goal xs (Just st, Just d)  = putStrLn ( "Solving grid using the " ++ show st            ++ " algorithm and the " ++ show d                ++ " distance" )  >>  runSearch goal [xs]  PQ.empty  S.empty  ( getNextNodes goal d                st            )  0 0 1

    solve' :: Grid -> Grid -> (Maybe SearchType, Maybe Distance) -> Int
    solve' goal xs (Nothing, Nothing) = runSearch' goal [xs]  PQ.empty  S.empty  ( getNextNodes goal defaultHeuristic defaultSearch )  0 0 1
    solve' goal xs (Just st, Nothing) = runSearch' goal [xs]  PQ.empty  S.empty  ( getNextNodes goal defaultHeuristic st            )  0 0 1
    solve' goal xs (Nothing, Just d)  = runSearch' goal [xs]  PQ.empty  S.empty  ( getNextNodes goal d                defaultSearch )  0 0 1
    solve' goal xs (Just st, Just d)  = runSearch' goal [xs]  PQ.empty  S.empty  ( getNextNodes goal d                st            )  0 0 1
