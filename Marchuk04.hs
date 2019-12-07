{-# OPTIONS_GHC -Wall #-}
module Marchuk04 where

import Data.Char(digitToInt)

-- 1.	Функцію analyseG,  яка реалізує синтаксичний аналіз мови, 
--      котра породжується  наступною КВ-граматика G = (VN, VT, P, S)
--      VN = {S, A}, VT = {a, b},  P = {S -> aSbAa, S -> b, A -> baAS, A -> a}. 
--      Можна скористатися функцією match, що визначена в допоміжному файлі, 
--      і визначити допоміжні функції s і a, котрі розпізнають слова, що виводяться з нетерміналів S і A відповідно. 

analyseG :: String -> Bool 
analyseG str = case s str of
                    Just st2 -> null st2
                    Nothing -> False


s :: String -> Maybe String
s ('a':st) = case s st of
                    Just ('b':st1) -> case a st1 of
                                            Just ('a':st2) -> Just st2
                                            _              -> Nothing
                    _             -> Nothing

s('b':str) = Just str

s _ = Nothing

a :: String -> Maybe String
a ('b':'a':str) = case a str of
                        Just str2 -> s str2
                        _ -> Nothing
    
a('a':str) = Just str

a _ = Nothing
   
-- 2.	Написать функція balance str, котра в рядку str розпізнає мову (задану граматикою G0), слова якої мають баланс дужок. 
balance :: String -> Bool
balance st1 = case b st1 of 
                         Just st2 -> null st2
                         Nothing -> False


b :: String -> Maybe String 
b('(':st1) =  case b st1 of 
    Just(')':st2) -> b st2
    _             -> Nothing  
b('[':st1) =  case b st1 of 
    Just(']':st2) -> b st2
    _             -> Nothing
b('{':st1) =  case b st1 of 
    Just('}':st2) -> b st2
    _             -> Nothing  
b st1 =  c st1

c :: String -> Maybe String 
c (' ':st1) = case c st1 of
                         Just(' ':st2) -> c st2
                         Just st2 -> b st2
                         _ -> Nothing

c st = Just st 

-- 3.    Функція-аналізатор analyseExpr розпізнає рядок, що містить синтаксично вірний арифметичний вираз
analyseExpr :: String -> Bool 
analyseExpr st1 = case ae st1 of 
                              Just st2 -> null st2
                              _ -> False

ae :: String -> Maybe String 
ae st1 = case af st1 of
    Just st2 -> aa st2
    Nothing -> Nothing

aa :: String -> Maybe String 
aa (p:st1) | elem p "+-*"  = case af st1 of
    Just st2 -> aa st2
    Nothing -> Nothing
   
aa st1 = Just st1

af :: String -> Maybe String 
af ('(':st1) = case ae st1 of
    Just(')':st2) -> Just st2
    _ -> Nothing

af(p:st1)| elem p "1234567890" = Just st1

af _ = Nothing

-- 4.	Функції evalLeft розпізнає синтаксично вірний арифметичний вираз, 
--      повертаючи його ціле значення v – Just v  або Nothing в іншому випадку. 
--      Значення виразу обчислюється в припущенні, що всі операції лівоасоціативні і мають однаковий пріоритет.
evalLeft :: String -> Maybe Int 
evalLeft st1 = case le st1 of 
                    Just (v,st2)| null st2 -> Just v 
                    _                      -> Nothing 

le :: String -> Maybe (Int,String) 
le st1 =  case lf st1 of 
    Just (v1,st2) -> la (v1,st2) 
    Nothing       -> Nothing  

la :: (Int,String) -> Maybe (Int,String) 
la (v1,(d:st1))| elem d "+-*" = case lf st1 of
    Just (v2,st2) -> la ((inOp d v1 v2),st2)
    Nothing       -> Nothing 

la (v1,st1)                  = Just (v1,st1)

lf :: String -> Maybe (Int,String)
lf ('(':st1) = case le st1 of 
    Just (v,(')':st2)) -> Just (v,st2) 
    _                  -> Nothing  

lf (d:st1) | elem d "1234567890" = Just (digitToInt d,st1) 

lf _            = Nothing

-- 5.	Функції  evalRigth схожа на попередню evalLeft, 
--      але значення виразу обчислюється в припущенні, 
--      що всі операції правоасоціативні і мають однаковий пріоритет. 
evalRigth :: String -> Maybe Int 
evalRigth st1 = case re st1 of 
                    Just (v,st2)| null st2 -> Just v 
                    _                      -> Nothing 

re :: String -> Maybe (Int,String)
re st1 =  case rf st1 of 
    Just (v1,st2) -> ra (v1,st2) 
    Nothing       -> Nothing

ra :: (Int,String) -> Maybe (Int,String)
ra (v1,(d:st1))| elem d "+-*" = case re st1 of
    Just (v2,st2) -> ra ((inOp d v1 v2),st2)
    Nothing       -> Nothing 

ra (v1,st1)                  = Just (v1,st1)

rf :: String -> Maybe (Int,String)
rf ('(':st1) = case re st1 of 
    Just (v,(')':st2)) -> Just (v,st2) 
    _                  -> Nothing  

rf (d:st1) | elem d "1234567890" = Just (digitToInt d,st1) 

rf _            = Nothing

-- 6.	Функція evalPrior схожа на попередню evalLeft, але значення виразу обчислюється в припущенні, 
--      що всі операції лівоасоціативні, але операції  ‘+’, ‘-‘ мають однаковий пріоритет, 
--      котрий менший за пріоритет операції  ‘*’.
evalPrior :: String -> Maybe Int 
evalPrior st1 = case pe st1 of 
                    Just (v,st2)| null st2 -> Just v 
                    _                      -> Nothing 

pe :: String -> Maybe (Int,String)
pe st1 =  case pt st1 of 
    Just (v1,st2) -> pa (v1,st2) 
    Nothing       -> Nothing

pa :: (Int,String) -> Maybe (Int,String) 
pa (v1,(d:st1))| elem d "+-" = case pt st1 of
    Just (v2,st2) -> pa ((inOp d v1 v2),st2)
    Nothing       -> Nothing 

pa (v1,st1)                  = Just (v1,st1)

pt :: String -> Maybe (Int,String) 
pt st1 =  case pf st1 of 
    Just (v1,st2) -> pb (v1,st2) 
    Nothing       -> Nothing

pb :: (Int,String) -> Maybe (Int,String) 
pb (v1,(d:st1))| elem d "*" = case pf st1 of
    Just (v2,st2) -> pb ((inOp d v1 v2),st2)
    Nothing       -> Nothing 

pb (v1,st1)                  = Just (v1,st1)

pf :: String -> Maybe (Int,String) 
pf ('(':st1) = case pe st1 of 
    Just (v,(')':st2)) -> Just (v,st2) 
    _                  -> Nothing  

pf (d:st1) | elem d "1234567890" = Just (digitToInt d,st1) 

pf _            = Nothing

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String 
match cm (Just (t:st)) | cm==t = Just st
match _ _                    = Nothing 

inOp :: Char -> Int -> Int -> Int
inOp cs = case cs of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}