{-# OPTIONS_GHC -Wall #-}
module Marchuk10 where

import Data.List
import qualified Text.ParserCombinators.Parsec as P
import Data.Maybe

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- 1.	Функція simplify re, котра вилучає всі + або ? з виразу re  використовуючи правила спрощення. 
simplify :: RE -> RE  
simplify (Null) = Null
simplify (Term ch) = (Term ch)
simplify (Seq re11 re22) = (Seq (simplify re11) (simplify re22))
simplify (Alt re11 re22) = (Alt (simplify re11) (simplify re22))
simplify (Rep re) = (Rep (simplify re))
simplify (Plus re) = (Seq (simplify re) (Rep (simplify re)))
simplify (Opt re) = (Alt (simplify re) Null)

-- 2.	Предикат isTerminal aut s і предикат isEssential aut s, 
--        що повертає значення True тоді і тільки тоді, коли стан s являється заключним станом або суттєвим станом, 
--        відповідно, автомату aut. Стан s - суттєвий коли він або заключний 
--        або автомат aut в цьому стані можна прочитати на вході деякий символ (тобто існує перехід виду (s,t,C c)).
isTerminal :: Automation -> State -> Bool 
isTerminal aut s = isLast aut s

isEssential :: Automation -> State -> Bool 
isEssential aut s = isLast aut s || canReadChar aut s

isLast :: Automation -> State -> Bool
isLast (_,ls,_) s = elem s ls

canReadChar :: Automation -> State -> Bool
canReadChar (_,_,[]) _ = False
canReadChar (st,sts,ts) s | checkState (head ts) s = True
                          | otherwise = canReadChar (st,sts,(tail ts)) s

checkState :: Transition -> State -> Bool
checkState (ss,_,C _) s = ss == s 
checkState (_,_,Eps) _ = False

getSnd :: Transition -> State
getSnd (_,s,_) = s
     
getFst :: Transition -> State
getFst (s,_,_) = s
     
getTrd :: Transition -> Label
getTrd (_,_,l) = l

-- 3.	Функція transitionsFrom aut s, котра повертає список переходів, що виходять з стану s в автоматі aut.
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom (_,_,ts) s = filter (\(x,_,_) -> s == x) ts

-- 4.	Функція  labels trx, котра повертає всі мітки без дублікатів, 
--        що з’являються в списку переходів trx. Довільні Eps потрібно вилучити з результату.
labels :: [Transition] -> [Label]
labels [] = []
labels ((_,_,Eps):ts) = labels ts
labels ((_,_,C ch):ts) = nub ((C ch) : labels ts)

-- 5.	Функція  acceptsDA daut st, котра повертає  True, 
--        в тому і тільки в тому випадку коли автомат детермінований daut допускає рядок st.
acceptsDA :: Automation -> String -> Bool
acceptsDA aut@(ss,_,_) str = processString ss aut str

processString :: State -> Automation -> String -> Bool
processString 1000 _ _ = True
processString (-10) _ _ = False
processString ss aut str = processString (stepDa ss aut str) aut (tail str)

stepDa :: State -> Automation -> String -> State
stepDa s (_,ls,ts) [] | elem s ls = 1000
                      | elem s (map (\(x,_,Eps) -> x) ts) =  head (map (\(_,y,Eps) -> y) ts)
                      | otherwise = -10

stepDa s (_,ls,ts) (st:_) | elem s ls = -10
                           | null(filter (\(x,_,C ch) -> x == s && ch == st) ts) = -10
                           | otherwise = getSnd (head (filter (\(x,_,(C ch)) -> x == s && ch == (st)) ts))                      
-- 6.	Функція  stStep naut st mc, котра обчислює множину станів, 
--        в які може перейти недетермінований автомат naut  зі стану st за один крок, 
--        прочитавши символ ‘с’, якщо mc == C ‘c’, або по порожньому переходу, якщо mc == Eps.
----------------------------
--        Функція setStep naut bs mc, котра обчислює множину станів,
--        в які може перейти недетермінований автомат naut  з одного із стану bs за один крок, 
--        прочитавши символ ‘с’, якщо mc == C ‘c’, або по порожньому переходу, якщо mc == Eps. 
----------------------------
--        Функція closure naut ss, котра обчислює множину станів, 
--        в які недетермінований автомат naut  може перейти з довільного стану з ss, 
--        не читаючи на вході жодного символу  (лише по порожнім переходам).

stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep (_,_,ts) s mc = map (\(_,x,_) -> x) (filter (\(x,_,z) -> s == x && z == mc) ts)
setStep naut st mc = foldl (++) [] (map (\x -> stStep naut x mc) st)
closure naut st = let step = setStep naut st Eps 
                   in if null(filter (\x -> notStateElem x st) step) then st else sort(nub (closure naut (step ++ st)))

notStateElem :: State -> [State] -> Bool
notStateElem st sts = null [x |x <- sts, x == st]

-- 7.	Функція  accepts aut st, котра повертає  True, в тому і тільки в тому випадку коли автомат aut допускає рядок st.
--        Для реалізації можна використати функції setStep i closure. 
accepts :: Automation -> String -> Bool
accepts aut@(ss,ls,_) str = not (null (filter (\x -> elem x ls) (processNautStr aut str (closure aut [ss]))))

processNautStr :: Automation -> String -> [State] -> [State]
processNautStr _ [] res = res
processNautStr aut str res = let tmp = closure aut (setStep aut res (C (head str)))
                                         in (processNautStr aut (tail str) tmp) ++ tmp

-- 8.	Функція  make re beg fin nxt, котра за регулярним виразом re будує НСА 
--        з початковим станом  beg і заключним fin використовуючи при необхідності нові стани починаючи з nxt. 
--        Результат функції - пара (trx,nxt1): trx – список переходів, 
--        що реалізують регулярний вираз  re, і nxt1- номер наступного стану.
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make (Term ch) beg fin nxt = ([(beg, fin, C ch)], nxt)
make Null beg fin nxt = ([(beg, fin, Eps)], nxt)
make (Rep re) beg fin nxt = let (trs, st) =  make re nxt (nxt+1) (nxt+2)
                                (trs1, st1) = make Null (nxt+1) nxt st
                                (trs2, st2) = make Null beg fin st1
                                (trs3, st3) = make Null (nxt+1) fin st2
                                (trs4, resSt) = make Null beg nxt st3
                              in (foldl (++) [] [trs,trs1,trs2,trs3,trs4], resSt)
make (Alt re11 re22) beg fin nxt = let (trs, st) = make re11 nxt (nxt+1) (nxt+4)
                                       (trs1, st1) = make re22 (nxt+2) (nxt+3) st
                                       (trs2, st2) = make Null beg nxt st1 
                                       (trs3, st3) = make Null beg (nxt+2) st2
                                       (trs4, st4) = make Null (nxt+1) fin st3  
                                       (trs5, resSt) = make Null (nxt+3) fin st4
                                   in (foldl (++) [] [trs,trs1,trs2,trs3,trs4,trs5], resSt)
make (Seq re11 re22) beg fin nxt = let (trs, st) = make re11 beg nxt (nxt+2)
                                       (trs1, st1) = make re22 (nxt+1) fin st
                                       (trs2, resSt) = make Null nxt (nxt+1) st1
                                   in (foldl (++) [] [trs,trs1,trs2], resSt) 
make _ _ _ _ = undefined

-- 9.	Функція  parseReg st, котра виконує синтаксичний аналіз рядка st, розпізнаючи регулярний вираз - значення типу  RE. 
--        (Можна використати модуль  Parsec).
parseReg :: String -> Maybe RE 
parseReg reStr = case P.parse initParse "" (filter (\ch -> (notElem ch "\t\n ")) reStr) of
                    Left _ -> Nothing
                    Right re -> Just re

initParse :: P.Parser RE
initParse = do re <- findAltRE
               P.eof
               return re;

findAltRE :: P.Parser RE
findAltRE = do re <- reTerm
               re11 <- P.many (sndAlt)
               if null re11 then return re else return (Alt re (reAlt re11))
           
reAlt :: [RE] -> RE
reAlt [] = undefined 
reAlt re | length re == 1 = head re
         | otherwise = Alt (head re) (reAlt (tail re))

sndAlt :: P.Parser RE
sndAlt = do _ <- P.char '|'
            re <- reTerm
            return re

reTerm :: P.Parser RE
reTerm = do re <- P.many1 operParse
            return (reSeq re)

reSeq :: [RE] -> RE
reSeq [] = undefined
reSeq re | length re == 1 = head re
         | otherwise = Seq (head re) (reSeq (tail re))

operParse :: P.Parser RE
operParse = do str <- initAlt
               opers <- P.many (P.oneOf "*?+")
               return (parseAllOps str (reverse opers))

parseAllOps :: RE -> String -> RE
parseAllOps re [] = re
parseAllOps re opers | (head opers) == '+' = Plus (parseAllOps re (tail opers))
                     | (head opers) == '*' = Rep (parseAllOps re (tail opers))
                     | otherwise = Opt (parseAllOps re (tail opers))

initAlt :: P.Parser RE
initAlt = (P.try findSymbols) P.<|> skipBrackets

skipBrackets :: P.Parser RE
skipBrackets = do _ <- P.char '('
                  re <- findAltRE
                  _ <- P.char ')'
                  return re

findSymbols :: P.Parser RE
findSymbols = do symbs <- P.noneOf "()|*+?"
                 return (Term symbs)

-- 10.	Функція  makeDA nda, котра перетворює НСА nda в еквівалентний детермінований автомат. 
--        Для реалізації можна визначити допоміжну функцію makeDA’, що визначена в допоміжному файлі. 
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut@(st,_,_) = let (st1,_,ts) = until endCycle (cycleIter aut) ([],[filter (\s -> isEssential aut s) (closure aut [st])],[])
                         in (head(st1), st1, ts)

endCycle :: ([MetaState], [MetaState], [MetaTransition]) -> Bool
endCycle (_,enmst,_) = null enmst

cycleIter :: Automation -> ([MetaState], [MetaState], [MetaTransition]) -> ([MetaState], [MetaState], [MetaTransition])
cycleIter aut (smst,enmst,mts) = let tmp = nub $ concatMap (\x -> labels $ transitionsFrom aut x) (head enmst)
                                     tmpMst = map (\x -> closure aut (setStep aut (head enmst) x)) tmp
                                     rsMst = map (\x -> filter (isEssential aut) x) tmpMst
                                     metaAut= (smst ++ [head enmst], tail enmst, mts)
                                   in newNormSt metaAut (head enmst) rsMst tmp

newNormSt :: ([MetaState], [MetaState], [MetaTransition]) -> MetaState -> [MetaState] -> [Label] -> ([MetaState], [MetaState], [MetaTransition])
newNormSt (mst,b,m) msx mlx l |null mlx = (mst,b,m)
                              |otherwise = newNormSt (mst, b' ,m') msx (tail mlx) (tail l) 
                                   where b' = if elem hm b || elem hm mst then b else b ++ [hm]
                                         m' = m ++ [(msx, hm, head l)]
                                         hm = head mlx
         
findInd :: Eq a => [a] -> a -> Int
findInd ls i = (fromMaybe (-2) $ elemIndex i ls) + 1

comparatorForStates :: (Ord a, Ord b) => (a, b, c) -> (a, b, d) -> Ordering
comparatorForStates (a, b, _) (a2, b2, _)   | a > a2 = GT
                                            | a < a2 = LT
                                            | otherwise = compare b b2

makeDA :: Automation -> Automation
makeDA (st,ls,ts) = let rsTs = sortBy comparatorForStates stLs
                        rsLs = [x + 1|x <- [0..(length lsmst) - 1], not (null (intersect (lsmst !! x) ls))]
                        (_,lsmst,mtrs) = makeDA' (st,ls,ts)
                        stLs = [(findInd lsmst mst, findInd lsmst enmst, mts) |(mst,enmst,mts) <- mtrs]
                     in  (1, rsLs, rsTs)
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )