{-# OPTIONS_GHC -Wall #-}
module Marchuk06 where

import Data.List

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- 1.	Предикат isOrdinary gr, що перевіряє чи задає список gr типа Graph неорієнтований граф. 
isOrdinary :: Graph -> Bool 
isOrdinary graph = notElem False [elemIsOrdinary graph x (graph!!x) | x <- [0 .. ((length graph) - 1)]]

elemIsOrdinary :: Graph -> Int -> [Int] -> Bool
elemIsOrdinary graph vertex edgs | elem vertex edgs = False
                                 | null edgs = True 
                                 | elem vertex (graph!!(head edgs)) = elemIsOrdinary graph vertex (tail edgs) 
                                 | otherwise =  False

-- 2.	Функція fromGraph gr, що перетворює представлення графу списком суміжності gr в стандартне.
fromGraph :: Graph -> GraphS 
fromGraph graph = (((length graph) - 1), edges graph)

nodes :: Graph -> [Int]
nodes g = [0 .. ((length g) -1)]

edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x <- nodes g, y <- g !! x]

-- 3.	Функція toGraph grS , що перетворює стандартне представлення графу grS в задання списком суміжності.
toGraph :: GraphS -> Graph 
toGraph graphs = [helpGraphS (snd graphs) x | x <- [0 .. fst graphs]]

helpGraphS :: [(Int,Int)] -> Int -> [Int]
helpGraphS edgs vertex = [snd x | x <- edgs, vertex == (fst x)]

-- 4.	Функція shortWay gr a b,  котра знаходить в неорієнтованому gr графі найкоротший шлях,
--      що з’єднує дві вершини графа a і b. Якщо в графі не існує шляху, що з’єднує вершини a і b,
--      то функція повертає порожній список []. 
shortWay :: Graph -> Int -> Int -> [Int] 
shortWay graph v1 v2 = let ways = sort ((shWayHelp (allWays graph v1) v2))
                        in shortWayTmp ways (reverse (head ways))

shortWayTmp :: [[Int]] -> [Int] -> [Int]
shortWayTmp ways opt | null ways = opt
                     | null opt = shortWayTmp (tail ways) (reverse (head ways))
                     | length (head ways) < length opt = shortWayTmp (tail ways) (reverse (head ways))
                     | otherwise = shortWayTmp (tail ways) opt
            

shWayHelp :: [[[Int]]] -> Int -> [[Int]]
shWayHelp ways v2 | null ways = [[]]
                  | null (head ways) = shWayHelp (tail ways) v2
                  | otherwise = shWayHelp (tail ways) v2 ++ (shWayTmp (head ways) v2)

shWayTmp :: [[Int]] -> Int -> [[Int]]
shWayTmp ways v2 | null ways = []
                 | null (head ways) = shWayTmp (tail ways) v2
                 | elem v2 (head ways) = shWayTmp (tail ways) v2 ++ [(head ways)]
                 | otherwise = shWayTmp (tail ways) v2

adj :: Graph -> Int -> [Int]
adj g v = g !! v

allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until stopCondition (oneStep gr) [[[v]]]

stopCondition :: [[[Int]]] -> Bool
stopCondition lst = null (head lst)

oneStep :: Graph -> [[[Int]]] -> [[[Int]]]
oneStep g (xs:xss) = let noCycle = filter (not . checkCycle) xs
                        in  [concatMap (\e -> (sameLengthWays g e))noCycle] ++ (xs:xss)

checkCycle :: [Int] -> Bool
checkCycle xs = if length xs == 1 then False else (length xs) /= ((length . nub) xs)

sameLengthWays :: Graph->[Int]->[[Int]]
sameLengthWays gr (x:xs) = let sosed = adj gr x
                       in [[y]++(x:xs)  | y <- sosed]
sameLengthWays _ [] = [[]]

-- 5.	Предикат isConnecting gr, що перевіряє чи є неорієнтований граф gr – зв’язним.
isConnecting :: Graph -> Bool 
isConnecting g = notElem False [not(null (shortWay g x y)) | x <- [0 .. length g - 1], y <- [0 .. length g - 1]]

-- 6.	Функція components gr , що знаходить всі зв’язні компоненти неорієнтованого графу gr.
components :: Graph -> [[Int]] 
components graph = nub [oneComponent graph x | x <- [0 .. length graph - 1]]

oneComponent :: Graph -> Int -> [Int]
oneComponent g v = [x | x <- [0 .. length g - 1], not(null (shortWay g v x))]

-- 7.	Функція eccentricity gr v, що обчислює ексцентриситету вершини v неорієнтованого зв’язного графа gr.
eccentricity :: Graph -> Int -> Int 
eccentricity g v = maximum [length(shortWay g v x) - 1 | x <- [0 .. length g - 1]]

-- 8.	Функції findDiameter gr і findRadius gr, котрі обчислюють, 
--      відповідно, діаметр і радіус неорієнтованого зв’язного графа gr. 
findDiameter :: Graph -> Int 
findDiameter g = maximum [eccentricity g x | x <- [0 .. length g - 1]]

findRadius :: Graph -> Int 
findRadius g = minimum [eccentricity g x | x <- [0 .. length g - 1]]

-- 9.	Функція findCenter gr, що знаходить список вершин, 
--      котрі утворюють центр неорієнтованого зв’язного графа gr.
findCenter :: Graph -> [Int] 
findCenter g = let radius = findRadius g
                    in [x | x <- [0 .. length g - 1], eccentricity g x == radius]

-- 10.	Функція shortWayAll gr a b,  котра знаходить в неорієнтованому gr графі 
--      всі різні найкоротші шляхи, що з’єднують дві вершини графа a і b .
shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays g v1 v2 = let shortLength = length (shortWay g v1 v2)
                     in map (reverse) (filter (\x -> length x == shortLength) (reverse (shWayHelp (allWays g v1) v2)))

--------------------- ������ ���� - ����� -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]