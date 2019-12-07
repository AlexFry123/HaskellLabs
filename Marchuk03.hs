{-# OPTIONS_GHC -Wall #-}
module Marchuk03 where

type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving Show
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- 1.	Функція isPrefix bs xs , що перевіряє,  чи являється слово bs - префіксом (початком) xs
isPrefix :: String -> String -> Bool 
isPrefix xs ys  | null xs            = True
                | null ys            = False
                | head xs == head ys = isPrefix (tail xs) (tail ys)
                | otherwise          = False

-- 2.	Функція substitute sub i w, котра  застосовує (виконує) підстановку sub до слова w в позиції i
substitute :: Substitution -> Int -> String -> String
substitute sb x xs | isPrefix (getFst sb) (drop x xs) = take x xs ++ (getSnd sb) ++ drop (x + (length (getFst sb))) xs
                   | otherwise = xs
getFst :: Substitution -> String
getFst (a,_,_) = a

getSnd :: Substitution -> String
getSnd (_,a,_) = a

getTrd :: Substitution -> Bool
getTrd (_,_,a) = a

-- 3.	Функція findPosition w sub, що знаходить список [(sub,i1), …,  (sub,ik)] 
--      всіх можливих місць (i1 < … < ik) застосування підстановки sub до слова w.
findPosition :: String -> Substitution -> [(Substitution,Int)] 
findPosition str sub = [(sub,x)|x <- [0 .. length str], isPrefix (getFst sub) (drop x str)]

-- 4.	Функція findAll algo w  формує список всіх можливих застосувань підстановок алгоритму algo до слова w.
--      В списку спочатку ідуть всі можливі місця виконання першої підстановки, потім другої і т. д.
--      Для кожної підстановки позиції впорядковані від лівої до правої.
findAll :: Algorithm -> String -> [(Substitution,Int)]  
findAll alg str = findingAlgs alg str



findingAlgs :: Algorithm -> String -> [(Substitution,Int)]
findingAlgs alg str | null alg = []
                    | (not (null alg)) = findPosition str (head alg) ++ findAll (tail alg) str 
                    | otherwise = findAll (tail alg) str 
-- 5.	Функція stepA algo (bt,st,word) реалізує один крок (застосування одної підстановки) алгоритму algo 
--      на конфігурації  (bt,st,word), створюючи нову конфігурацію.
stepA :: Algorithm -> ConfigA -> ConfigA
stepA alg conf = ((not (getTrd (fst (head (findAll alg (getConfTrd conf)))))), ((getConfSnd conf) + 1), substitute (fst (head (findAll alg (getConfTrd conf)))) (snd (head (findAll alg (getConfTrd conf)))) (getConfTrd conf))

getConfTrd :: ConfigA -> String
getConfTrd (_,_,a) = a

getConfSnd :: ConfigA -> Int
getConfSnd (_,a,_) = a

getConfFst :: ConfigA -> Bool
getConfFst (a,_,_) = a

-- 6.	Функція evalA algo m word застосовує алгоритм algo до слова word, виконуючи не більше ніж m підстановок. 
--      Якщо процес обчислення не завершується за m кроків, то результат невизначений - Nothing.
evalA :: Algorithm -> Int -> String -> Maybe String 
evalA _ 0 _ = Nothing
evalA alg m str | not (getConfFst (stepA alg (False,0,str))) = Just (getConfTrd(stepA alg (False,0,str))) 
                | otherwise = evalA alg (m-1) (getConfTrd (stepA alg (False,0,str)))

-- 7.	Функція maximReg pr - знаходить найбільший номер регістра, що вживається в програмі pr.
maximReg :: Program -> Int 
maximReg prog = doMaxReg prog 0

doMaxReg :: [Command] -> Int -> Int
doMaxReg prog x | null prog = x
                | commandRep (head prog) > x = doMaxReg (tail prog) (commandRep (head prog))
                | otherwise = doMaxReg (tail prog) x

commandRep :: Command -> Int
commandRep (Z x) = x
commandRep (S x) = x
commandRep (T x y) = max x y
commandRep (J x y _) = max x y

-- 8.	Функція ini pr ir - за програмою pr та значенням перших регістрів ir, формує початкове значення списку регістрів. 
--      Функція upd reg r v - змінює значення списку регістрів reg, 
--      встановлюючи значення елементу списку з номером r (r≥0) рівним v. Елементи списку нумеруються (індексуються) з 0.
ini :: Program -> [Int] -> [Int] 
ini pr reg | (maximReg pr) > (length reg) = ini pr (reg ++ [0])
           | otherwise = reg

upd :: [Int] -> Int -> Int-> [Int]
upd reg ind val = (take ind reg) ++ [val] ++ (drop (ind + 1) reg)

-- 9.	Функція stepC pr (nm,st,rg) реалізує один крок виконання програми МНР, тобто
--      виконання однієї команди програми pr в конфігурації  (nm,st,rg). 
stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm,st,rg) = (snd (makeCom (pr!!(nm-1)) rg nm), st+1, fst (makeCom (pr!!(nm-1)) rg nm))

makeCom :: Command -> [Int] -> Int -> ([Int],Int)
makeCom (Z x) reg nm = ((upd reg (x-1) 0),(nm+1))
makeCom (S x) reg nm = ((upd reg (x-1) ((reg!!(x-1))+1)), (nm+1))
makeCom (T x y) reg nm = ((upd reg (y-1) (reg!!(x-1))), (nm+1))
makeCom (J x y nm1) reg nm = if (reg!!(x-1))==(reg!!(y-1)) then (reg,nm1) else (reg,(nm+1))

-- 10.	Функція evalC pr mx ir - виконує програму pr на початкових даних ir, 
--      виконується не більше ніж mx кроків (команд). Якщо машина НЕ зупиниться, то результат Nothing.
evalC :: Program -> Int -> [Int] -> Maybe Int 
evalC _ 0 _ = Nothing
evalC pr mx rg = tempEval pr (mx+1) rg 1

tempEval :: Program -> Int -> [Int] -> Int -> Maybe Int
tempEval _ 0 _ _ = Nothing
tempEval pr mx rg num = if (num > (length pr)) then Just (head rg) else tempEval pr (mx-1) (getReg (stepC pr (num,mx,rg))) (getNm (stepC pr (num,mx,rg)))


getNm :: ConfigC -> Int
getNm (a,_,_) = a

getStep :: ConfigC -> Int
getStep (_,a,_) = a

getReg :: ConfigC -> [Int]
getReg (_,_,a) = a

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm 
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ] 

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ] 
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False) 
          , ("db", "bd", False) 
          , ("d", "", True) 
          , ("caa", "aca", False) 
          , ("cab", "bca", False) 
          , ("cba", "acb", False)
          , ("cbb", "bcb", False) 
          , ("", "c", False) 
          ]

-- добуток натуральних чисел 
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program 
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1] 

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y 
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]