{-# OPTIONS_GHC -Wall #-}
module Marchuk08 where

import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

data Recur = Zero | Succ | Sel Int Int 
           | Super Recur [Recur] 
           | Prim Recur Recur 
           | Mini Recur Int 
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]  

-- 1.	Функція isNumbConst syst f, котра перевіряє що f - є “функція-число” в системі рекурсивних функцій syst.
isNumbConst :: System -> Recur -> Bool 
isNumbConst syst f = (tmpConst syst f) && (evRank syst f)==1

tmpConst :: System -> Recur -> Bool 
tmpConst syst (Name str) = let fc = findFunc syst str
                               in zeroConstCheck fc || secondConstCheck fc syst || thirdConstCheck (Just str, fc) syst
tmpConst syst f = (zeroConstCheck (Just f) || secondConstCheck (Just f) syst || thirdConstCheck (findName syst f, Just f) syst)

findName :: System -> Recur -> Maybe String
findName [] _ = Nothing
findName syst f|snd (head syst) == f = Just (fst (head syst))
               |otherwise = findName (tail syst) f

findFunc :: System -> String -> Maybe Recur
findFunc [] _ = Nothing
findFunc syst nm|fst (head syst) == nm = Just (snd (head syst))
                |otherwise = findFunc (tail syst) nm

zeroConstCheck :: Maybe Recur -> Bool
zeroConstCheck Nothing = False
zeroConstCheck f = fromJust f == Zero

secondConstCheck :: Maybe Recur -> System -> Bool
secondConstCheck Nothing _ = False
secondConstCheck (Just (Name str)) syst = secondConstCheck (findFunc syst str) syst
secondConstCheck f syst = let n = useRecur (fromJust f) [0,0,0] syst
                      in useRecur (Super Succ [fromJust f]) [n] syst == (n+1)

thirdConstCheck :: (Maybe String,Maybe Recur) -> System -> Bool
thirdConstCheck(Nothing,_) _ = False
thirdConstCheck(_,Nothing) _ = False
thirdConstCheck (nm,f) syst = zeroConstCheck (Just (Name (fromJust nm))) || secondConstCheck (Just (Name (fromJust nm))) syst 

useRecur :: Recur -> [Int] -> System -> Int
useRecur Zero _ _ = 0
useRecur Succ st _ = (head st) + 1
useRecur (Sel i a) st _ = st!!(a-1)
useRecur (Super f arg) st syst = useRecur f (map (\x -> useRecur x st syst) arg) syst
useRecur (Name str) st syst = useRecur (fromJust (findFunc syst str)) st syst
useRecur (Prim g h) rg syst = eval syst (Prim g h) rg
-- 2.	Функція evRank syst f, що обчислює ранг (арність) функції f в системі syst.
evRank :: System -> Recur -> Int 
evRank _ Zero = 1
evRank _ Succ = 1
evRank _ (Sel n k) = n
evRank syst (Super b al) = evRank syst (head al)
evRank syst (Prim i st) = ((evRank syst st) - 1)
evRank syst (Mini b n) = ((evRank syst b) - 1)
evRank syst (Name f) = evRank syst (fromJust (findFunc syst f))


-- 3.	Функція isNames syst, котра перевіряє що імена усіх функцій в системі syst визначені 
--    і вірно використовуються. Система syst = [(n1,f1), …, (nk,fk)] має вірні імена, 
--    якщо жодне ім»я не об»являється двічі і в означенні fi імені ni  використовуються лише імена  n1, n2, … ni-1. 
isNames :: System -> Bool 
isNames syst = notDuplicates (head syst) (tail syst) && rightUsage (last syst) (init syst)

notDuplicates :: (String,Recur) -> System -> Bool
notDuplicates _ [] = True
notDuplicates (nm,f) syst | elem nm (map fst syst) = False
                          | otherwise = notDuplicates (head syst) (tail syst)

rightUsage :: (String,Recur) -> System -> Bool
rightUsage (nm,f) [] = isRight (nm,f) []
rightUsage (nm,f) syst | isRight (nm,f) syst = rightUsage (last syst) (init syst)
                       | otherwise = False

isRight :: (String,Recur) -> System -> Bool
isRight (nm,f) [] = null (names f)
isRight (nm,f) syst = null (filter (\x -> notElem x (map fst syst)) (names f))

listFunc :: [Recur] -> [String]
listFunc [] = []
listFunc (f:[]) = names f
listFunc (f:fs) = names f ++ (listFunc fs)

names :: Recur -> [String]
names (Sel n i)|n<i = ["errorSel"]
names (Super f fs) = names f ++ listFunc fs
names (Prim f1 f2) = names f1 ++ names f2
names (Mini b _) = names b
names (Name str) = [str]
names _ = []
-- 4.	Функція isRecur syst f, котра перевіряє, що означення функції f в системі syst немає помилок (вірне).
isRecur :: System -> Recur -> Bool
isRecur syst (Name str) = elem str (map fst syst)
isRecur syst f = (elem f (map snd syst)) || rightUsage ("tmp",f) syst

-- 5.	Функція eval syst f vl, що обчислює примітивно рекурсивну функцію f в системі syst на наборі даних vl. 
eval :: System -> Recur -> [Int] -> Int 
eval _ (Mini _ _) _ = undefined
eval syst f rg = tmpSix syst f rg

tmpSix :: System -> Recur -> [Int] -> Int 
tmpSix _ Zero _ = 0
tmpSix _ Succ rg = head rg + 1 
tmpSix syst (Name nm) rg = case (findFunc syst nm) of
                             Nothing -> error "There is no function with such name"
                             Just f -> tmpSix syst f rg 

tmpSix _ (Sel a b) rg |a<b = error "Wrong selector function"
                     |b<=(length rg) =  rg!!(b-1)
                     |otherwise = 0 
                  
tmpSix syst (Super g gs) rg = tmpSix syst g (map (\x -> tmpSix syst x rg) gs)
tmpSix syst p@(Prim g rg) reg |lst syst p reg == 0  = tmpSix syst g reg
                          |otherwise =  tmpSix syst rg (func2++[func1]++[func3])
                              where func1 = (lst syst p reg) - 1
                                    func2 = take (evRank syst p - 1) reg  
                                    func3 = tmpSix syst p (func2++[func1])

tmpSix _ (Mini f count) _ = undefined

lst:: System -> Recur -> [Int] -> Int 
lst s rg reg = let ev = evRank s rg 
               in if ev==(length reg) then (!!) reg (ev-1)
                         else 0

-- 6.	Функція evalPart syst f vl, котра обчислює частково-рекурсивну функцію (не всюди визначену) f 
--    в системі syst на наборі даних vl.  Якщо результат функції невизначено, то  повертається значення Nothing.  
evalPart :: System -> Recur -> [Int] -> Maybe Int 
evalPart syst (Name str) rg = tmpPart syst (fromJust (findFunc syst str)) rg
evalPart syst (Mini f n) rg = tmpPart syst (Mini f n) rg
evalPart syst f rg = Just (tmpSix syst f rg)

tmpPart :: System -> Recur -> [Int] -> Maybe Int 
tmpPart syst (Mini f n) rg = let result = filter (\x -> 0==(eval syst f (rg ++ [x])))[0..n]
                                 in if null result then Nothing else Just (head result)

tmpPart syst f rg = Just (tmpSix syst f rg)
-- 7.	Функція parseRec str  виконує синтаксичний аналіз рядка str, 
--    розпізнаючи набір рекурсивних функцій. (Можна використати модуль  Parsec).
parseRec :: String -> Maybe System 
parseRec systStr = tmpParseRec systStr

tmpParseRec :: String -> Maybe System
tmpParseRec systemString = let str = filter (/='\t')(filter (/=' ') (filter (/='\n') systemString))
                              in case parse system "" str of 
                                       Left _ -> Nothing
                                       Right recur -> Just recur

number::Parser Int
number = do x <- many1 digit
            return $ read x

identifyNameRecur :: Parser Recur
identifyNameRecur = do ch<-letter
                       chs <- many (digit <|> letter)
                       return (Name (ch:chs))
         
identifyName :: Parser String
identifyName = do ch<-letter
                  chs <- many (digit <|> letter)
                  return (ch:chs)

operators::Parser Recur
operators = (functions<|>super<|>prim<|>mini)
  
functions :: Parser Recur
functions =(try a1)<|>(try z1)<|>(try sel)<|>identifyNameRecur
 
z1, a1, sel ::Parser Recur
a1 = do  _<-string "a1"
         return Succ

z1 = do _<-string "z1"
        return Zero

sel = do _<-char 's'
         n<-digit
         i<-digit
         return (Sel (read [n]) (read [i]))

super, prim, mini :: Parser Recur
super = do _<-char '('
           g <- operators
           _ <- char ':'
           gg<-operators
           ggs<- many (do {_ <-char ','; rec <- operators; return rec}) 
           _ <- char ')'
           return (Super g (gg:ggs)) 
prim = do _<- char '['
          g<-operators
          _<-char ','
          f<-operators
          _<-char ']'
          return (Prim g f)

mini = do _<- char '{'
          f<-operators
          _<-char ','
          i<-number
          _<-char '}'
          return (Mini f i)

system :: Parser System
system = do x<- many myParse
            eof
            return x;
            
myParse :: Parser (String, Recur)
myParse = do name<- identifyName
             _<- char '='
             func<-operators
             _<-char ';'
             return (name,func)


--------------------- ������ ���� -  -------
syst1, syst2 :: System 
syst1 = [("const0", Zero)   
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]]) 
   , ("const2", Super Succ [Super Succ [Zero]]) 
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ])) 
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))  
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))  
   , ("subtract1", Prim Zero (Sel 2 1))  
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))  
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])     
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])  
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])  
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)    
   ]
   
syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String    
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	      \  notSignum = [(a1:z1),(z1:s21)];\n\
		  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
		  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"
 
sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"