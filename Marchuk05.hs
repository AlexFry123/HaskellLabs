{-# OPTIONS_GHC -Wall #-}
module Marchuk05 where

import Data.Char(isUpper)
import Data.List
import Data.Maybe

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця 

-- 1.	Функція addOne st c - додає до множини-рядка st символ c.
--     Функція addAll st wd - додає до множини-рядка st символи з рядка wd,повертаючи  множину-рядок  (wd - не обов"язково множина-рядок).
--     Функція addWithout st wd – схожа на попередню але символ '$' з рядка wd НЕ додається (ігнорується).                                                                                             Функція inter st1 st2 - реалізує перетин двох множин-рядків st1 i st2, повертаючи множину-рядок.
addOne :: String -> Char -> String  
addOne str ch | (elem ch str) = str 
              | otherwise     = str ++ [ch]

addAll :: String -> String -> String 
addAll str1 str2 | null str2 = str1 
                 | otherwise = addAll (addOne str1 (head str2)) (tail str2)

addWithout :: String -> String -> String 
addWithout str1 str2 | null str2            = str1 
                     | ((head str2) == '$') = addWithout str1 (tail str2) 
                     | otherwise            = (addWithout (addOne str1 (head str2)) (tail str2))

inter :: String -> String -> String 
inter str1 str2 = sort (intersectionWithoutReverse str1 str2)

intersectionWithoutReverse :: String -> String -> String 
intersectionWithoutReverse str1 str2 | ((null str2) || (null str1))  = "" 
                                     | elem (head str1) str2         = (intersectionWithoutReverse (tail str1) str2) ++ [head str1] 
                                     | otherwise                     = intersectionWithoutReverse (tail str1) str2

-- 2.	Функція tkPredict pt n вибирає з прогнозуючої таблицi pt  множину, що  зв"язана з нетерміналом n. 
--     Якщо нетермінал n відсутній, то повертається "" - порожня множина.
--     Функція upPredict pt n st змінює в таблиці pt множину,  зв"язану з нетерміналом n, на рядок-множину st. 
--     Якщо нетермінал n відсутній, то в таблицю pt додається пара (n,st).
tkPredict :: Predict -> Char -> String 
tkPredict predict ch | null predict                 = "" 
                     | (fst (head predict)) == ch   = snd (head predict) 
                     | otherwise                    = tkPredict (tail predict) ch

upPredict :: Predict -> Char -> String -> Predict 
upPredict predict ch str = sortTask2 (predictList predict ch str)

predictList :: Predict -> Char -> String -> Predict 
predictList predict ch str | notElem ch (map (fst) predict) = insertPredict predict (ch,str) 
                           | (fst (head predict)) == ch     = upPredict (tail predict) ch str 
                           | otherwise                      = upPredict ((tail predict) ++ [head predict]) ch str

sortTask2 :: Predict -> Predict
sortTask2 predict = foldl insertPredict [] predict

insertPredict :: Predict -> (Char,String) -> Predict
insertPredict [] (ch,str) = [(ch,str)]
insertPredict (y:ys) (ch,str) | ch < (fst y) = (ch,str) : y : ys 
                              | otherwise    = y : insertPredict ys (ch,str)
-- 3.	Функція parse gr ctl word, що моделює роботу LL(1)-аналізатора для граматики gr 
--     з управляючою таблицею ctl на слові word, повертаючи Just il, де  il- список номерів продукцій, 
--     що задають лівосторонній вивід слова  word, або Nothing - якщо слово word НЕ належить мові граматики  gr.
--     Можна додатково визначити функцію step gr ctl (input, staсk, result), 
--     що описує один крок роботи LL(1)-аналізатора з таблицею ctl на конфігурації (input, staсk, result). 
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse gram ctrl str = parseMyLL gram ctrl (str ++ "$") ([fst (head gram)] ++ "$") (Just [])

parseMyLL ::  Grammar -> Control -> String -> String -> Maybe [Int] -> Maybe [Int]
parseMyLL gram ctrl str stack res = if res == Nothing then Nothing else if (length stack) > 1 then parseMyLL gram ctrl (getFstStep (step gram ctrl (str, stack, res))) (getSndStep (step gram ctrl (str, stack, res))) (getTrdStep (step gram ctrl (str, stack, res))) else if ((str == "$") && (not (stack == "$"))) || ((not (str == "$")) && (stack == "$")) then Nothing else if (stack == "$") && (str == "$") then res else parseMyLL gram ctrl (getFstStep (step gram ctrl (str, stack, res))) (getSndStep (step gram ctrl (str, stack, res))) (getTrdStep (step gram ctrl (str, stack, res)))

step :: Grammar -> Control -> 
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step gram ctrl (input, stack, result) = if result == Nothing then (input,stack,Nothing) else if (head stack) == (head input) then ((tail input), (tail stack), result) else if elem ((head stack), (head input)) (map (fst) ctrl) then (input, (snd(gram!!(getElemFromCtrl ((head stack),(head input)) ctrl)) ++ (tail stack)), Just ((fromJust result) ++ [(getElemFromCtrl ((head stack),(head input)) ctrl)])) else (input, stack, Nothing)

getFstStep,getSndStep :: (String, String, Maybe [Int]) -> String
getFstStep (a,_,_) = a
getSndStep (_,a,_) = a

getTrdStep :: (String, String, Maybe [Int]) -> Maybe [Int]
getTrdStep (_,_,a) = a

getElemFromCtrl :: (Char, Char) -> Control -> Int
getElemFromCtrl (input, stack) ctrl = if ((fst (head ctrl)) == (input,stack)) then snd (head ctrl) else getElemFromCtrl (input,stack) (tail ctrl)

-- 4.	Функція first pFst st, котра визначає множину початкових термінальних символів,
--     котрі можна вивести з слова st (pFst - прогнозуюча таблиця початкових терміналів)
first :: Predict -> String -> String
first pred str = if null str then "$" else if not (isUpper (head str)) then [head str] else sort (getProgTable pred str)

getProgTable :: Predict -> String -> String
getProgTable pred str = if (length str) == 1 then getTerminal pred str else if notElem '$' (getTerminal pred str) then getTerminal pred [(head str)] else addAll (getProgTable pred (tail str)) (addWithout "" (getTerminal pred [(head str)]))
                                                                    

getTerminal :: Predict -> String -> String
getTerminal pred str = if null pred then [head str] else if ((fst (head pred)) == (head str)) then snd (head pred) else getTerminal (tail pred) str

-- 5.	Функція buildingControl gr pFst pNxt, що будує управляючу таблицю для LL(1)-граматики gr 
--     з прогнозуючими таблицями pFst  початкових і pNxt  наступних терміналів.
buildingControl :: Grammar -> Predict -> Predict -> Control 
buildingControl gram predFrst predNxt = sortTask5 (tempHelp gram predFrst predNxt 0) 

tempHelp :: Grammar -> Predict -> Predict -> Int -> Control 
tempHelp gram pFst pNxt n | null gram = []
                     | not ((first pFst (snd (head gram)))=="$") = (tempHelp  (tail gram) pFst pNxt (n+1)) ++ (oneControlPart (fst (head gram)) (first pFst (snd (head gram))) pFst n)
                     | (first pFst (snd (head gram)))=="$" = (tempHelp  (tail gram) pFst pNxt (n+1)) ++ (oneControlPart (fst (head gram)) (tkPredict pNxt (fst (head gram))) pNxt n)
                     | otherwise = tempHelp (tail gram) pFst pNxt (n+1)


oneControlPart ::  Char -> String -> Predict -> Int -> [((Char,Char),Int)] 
oneControlPart ch str pred num = if (fst (head pred))==ch then [((ch,ch1),num) |ch1 <- str] else oneControlPart ch str (tail pred) num

sortTask5 :: Control -> Control
sortTask5 ctrl = foldl sortTuple [] ctrl

sortTuple :: Control -> ((Char,Char),Int) -> Control
sortTuple [] ((ch1,char2),num) = [((ch1,char2),num)]
sortTuple (y:ys) ((ch1,char2),num) | ch1 < (fst (fst y)) = ((ch1,char2),num) : y : ys 
                                   | (ch1 == (fst (fst y))) && (char2 < (snd (fst y))) = ((ch1,char2),num) : y : ys
                                   | otherwise = y : sortTuple ys ((ch1,char2),num)

-- 6.	Функція testingLL1 gr pFst pNxt, що перевіряє, чи є граматика gr 
--     з прогнозуючими таблицями pFst – початкових і pNxt наступних терміналів - LL(1)-граматикою.  
--     Для реалізації можна додатково визначити функції: 
--
--	fromGrammar gr - групує всі продукції граматики gr в список пари (n,rls), 
--     де  n - нетермінал граматики і rls - ВСІ праві частини продукцій, що виводяться з n.
--------------------------------------------------------------------------------------------------- 
--	testFst rls - перевіряє 1 властивість для одного з нетерміналів граматики, тобто: 
--     з різних правих частин його продукцій  rls виводяться різні початкові термінали. 
--------------------------------------------------------------------------------------------------- 
--	testFollow fs rls – перевіряє 2  властивість для одного з нетерміналів граматики, 
--     коли з одної з продукцій нетерміналу rls виводиться порожнє слово ( fs - наступні символи для цього нетерміналу).

testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 gram pFst pNxt = if notElem False (map (\tupElem -> if testFst (mapForLL1 pFst tupElem) && (if elem "" (snd tupElem) then testFollow (tkPredict pNxt (fst tupElem)) (snd tupElem) else True) then True else False) (fromGrammar gram)) then True else False

mapForLL1 :: Predict -> (Char,[String]) -> [String]
mapForLL1 pred tup = map (first pred) (snd tup)

fromGrammar :: Grammar ->  [(Char,[String])]
fromGrammar gr = fromGramTemp gr []

fromGramTemp :: Grammar -> [(Char,[String])] -> [(Char,[String])]
fromGramTemp gram lst = if null gram then lst else if elem (fst (head gram)) (map (fst) lst) then fromGramTemp (tail gram) lst else fromGramTemp (tail gram) (lst ++ [(fst (head gram), createListFromGram gram (fst (head gram)))])

createListFromGram :: Grammar -> Char -> [String]
createListFromGram gram ch = if null gram then [] else if ch == (fst (head gram)) then createListFromGram (tail gram) ch ++ [snd (head gram)] else createListFromGram (tail gram) ch

testFst :: [String] -> Bool
testFst lstr = tempTestFst lstr 0

tempTestFst :: [String] -> Int -> Bool
tempTestFst lstr num = if (length lstr == num) then True else if (length lstr == 1) then True else if testFollow (head lstr) (tail lstr) then tempTestFst ((tail lstr) ++ [head lstr]) (num + 1) else False

testFollow :: String -> [String] -> Bool
testFollow str lstr = if null lstr then True else if null (inter str (head lstr)) then testFollow str (tail lstr) else False

-- 7.	Функція buildFst gr, котра будує для граматики gr прогнозуючу таблицю початкових терміналів.
--     Для реалізації можна додатково визначити функції: 
--	evalFst gr pFst - будує послідовність наближень прогнозуючої таблиці pFs
--	extandFst pFst (n,rul) - розширює прогнозуючу таблицю pFst, обробляючи продукцію (n,rul). 

buildFst :: Grammar -> Predict 
buildFst gram = helpBuildingFst gram (buildStart gram [])

helpBuildingFst :: Grammar -> Predict -> Predict
helpBuildingFst gram pred = if pred == (evalFst gram pred) then pred else helpBuildingFst gram (evalFst gram pred)

buildStart :: Grammar -> Predict -> Predict
buildStart gram pred = if null gram then pred else if (elem (fst (head gram)) (map (fst) pred)) && (snd (head gram)) == "" then buildStart (tail gram) (upPredict pred (fst (head gram)) "$") else if (elem (fst (head gram)) (map (fst) pred)) && not ( (snd (head gram)) == "") then buildStart (tail gram) (upPredict pred (fst (head gram)) "") else buildStart (tail gram) (upPredict pred (fst (head gram)) "")

evalFst :: Grammar -> Predict -> Predict 
evalFst gram pred = if null gram then pred else evalFst (tail gram) (extandFst pred (head gram))

extandFst :: Predict -> Production -> Predict 
extandFst pred (ch,str) = upPredict pred ch (sort (addAll (first pred (str)) (tkPredict pred ch)))

-- 8.	Функція buildNxt gr pFst, що будує для граматики gr прогнозуючу таблицю наступних терміналів, 
--     використовуючи ВЖЕ побудовану таблицю початкових терміналів pFst. 
--     Для реалізації можна додатково визначити функції: 
--
--	nontermTails gr  - будує ВСІ нетермінальні "хвости" продукцій граматики gr.
--------------------------------------------------------------------------------------------------- 
--	evalNxt tails pFst pNxt - будує послідовність наближень прогнозуючої таблиці наступних терміналів pNx, 
--     використовуючи: tails - ВСІ нетерміналні "хвости" продукцій граматики і pFst  - прогнозуючу таблицю початкових терміналів.
--------------------------------------------------------------------------------------------------- 
--	extandNxtOne pFst n pNxt (m:st) - розширює прогнозуючу таблицю pNxt обробляючи "хвіст" продукції граматики  (n,m:st).

buildNxt :: Grammar -> Predict -> Predict 
buildNxt gram pred = sort (helpBuildNxt (nontermTails gram) pred [])

nontermTails :: Grammar -> [(Char,String)] 
nontermTails gram = if null gram then [] else [(fst (head gram), findNonTermSymb(snd (head gram)) )] ++ (nontermTails (tail gram))

evalNxt :: [(Char,String)] -> Predict -> Predict -> Predict
evalNxt pred pFst pNxt = if null pred then pNxt else if null pNxt then evalNxt (tail pred) pFst [(fst (head pred), "$")] else if (elem (fst (head pred), "") pNxt)||(elem (fst (head pred), "$") pNxt) then evalNxt (tail pred) pFst pNxt else evalNxt (tail pred) pFst ([(fst (head pred) ,"")] ++ pNxt)

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne pFst ch pNxt (hs:st) = if not (isUpper hs) then extandNxtOne pFst ch pNxt st else if null st then ([(hs, sort (addAll(gramRule pNxt hs)(gramRule pNxt ch)))] ++ (delete (hs,(gramRule pNxt hs)) pNxt)) else if (notElem '$' (first pFst st)) then extandNxtOne pFst ch ([(hs,  sort (addAll(gramRule pNxt hs)(first pFst st)))] ++ (delete (hs,(gramRule pNxt hs)) pNxt)) st else extandNxtOne pFst ch ([(hs, sort (addAll(gramRule pNxt hs) (addWithout(gramRule pNxt ch)(first pFst st))))] ++ (delete (hs,(gramRule pNxt hs)) pNxt)) st
extandNxtOne _ _ pNxt [] =  pNxt

helpBuildNxt::[(Char,String)] -> Predict -> Predict -> Predict
helpBuildNxt gram pFst pNxt = if null pNxt then helpBuildNxt gram pFst (evalNxt gram pFst pNxt) else if (buildOneNextStep gram pFst pNxt) == pNxt then sort pNxt else buildOneNextStep gram pFst (buildOneNextStep gram pFst pNxt)

findNonTermSymb :: String -> String
findNonTermSymb str = if null str then "" else if not (isUpper (head str)) then findNonTermSymb (tail str) else str

buildOneNextStep::[(Char,String)]->Predict->Predict ->Predict
buildOneNextStep gram pFst pNxt = if null gram then pNxt else buildOneNextStep (tail gram) pFst (extandNxtOne pFst (fst (head gram)) pNxt (snd (head gram)))

gramRule::[(Char,String)]->Char->String
gramRule pred ch = case lookup ch pred of
                         Just s -> s
                         Nothing -> []

---------------------Тестові дані ---------------------------
 
gr0, gr1, gr2, gr3, gr4, gr5:: Grammar
--  LL(1)-граматики
gr0 = [('S',"aAS"),('S',"b"), ('A',"a"), ('A',"bSA")]  
gr1 = [('S',"TV"),('T',"d"),('T',"(S)"),('V',"+TV"),('V',"-TV"),('V',"")]  
gr2 = [('E',"TU"),('U',""),('U',"+TU"),('U',"-TU"),
       ('T',"FV"),('V',""),('V',"*FV"),('V',"%FV"),('V',"/FV"),
       ('F',"d"),('F',"(E)")] 
-- не LL(1)-граматики
gr3 = [('S',"aAS"), ('S',"a"),('A',"SbA"),('A',"ba"),('S',"")]
gr4 = [('E',"E+T"),('E',"T"), ('T',"T*F"), ('T',"F"), ('F',"d"),('F',"(E)") ]   
gr5 = [('E',"E+T"), ('E',"E-T"),('E',"T"), 
       ('T',"T*F"), ('T',"T%F"), ('T',"T/F"), ('T',"F"), 
       ('F',"d"),('F',"(E)") ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A',"ab"),('S',"ab")]
pFst1 = [('S',"(d"),('T',"(d"),('V',"$+-")]
pFst2 = [('E',"(d"),('F',"(d"),('T',"(d"),('U',"$+-"),('V',"$%*/")]
pFst3 = [('A',"ab"),('S',"$a")]
pFst4 = [('E',"(d"),('F',"(d"),('T',"(d")]
pFst5 = [('E',"(d"),('F',"(d"),('T',"(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A',"ab"),('S',"$ab")]
pNxt1 = [('S',"$)"),('T',"$)+-"),('V',"$)")]
pNxt2 = [('E',"$)"),('F',"$%)*+-/"),('T',"$)+-"),('U',"$)"),('V',"$)+-")]
pNxt3 = [('A',"$ab"),('S',"$b")]
pNxt4 = [('E',"$)+"),('F',"$)*+"),('T',"$)*+")]
pNxt5 = [('E',"$)+-"),('F',"$%)*+-/"),('T',"$%)*+-/")]   

-- управляючі таблиці 
ctl0, ctl1, ctl2 :: Control 
ctl0 = [(('A','a'),2),(('A','b'),3),(('S','a'),0),(('S','b'),1)]
ctl1 = [(('S','('),0),(('S','d'),0),(('T','('),2),(('T','d'),1),
        (('V','$'),5),(('V',')'),5),(('V','+'),3),(('V','-'),4)]
ctl2 = [(('E','('),0),(('E','d'),0),(('F','('),10),(('F','d'),9),
        (('T','('),4),(('T','d'),4),(('U','$'),1),(('U',')'),1),
        (('U','+'),2),(('U','-'),3),(('V','$'),5),(('V','%'),7),
        (('V',')'),5),(('V','*'),6),(('V','+'),5),(('V','-'),5),(('V','/'),8)]