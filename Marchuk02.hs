{-# OPTIONS_GHC -Wall #-}
module Marchuk02 where

-- 1.	Функція sumFl xs, котра вираховує суму цілих чисел – елементів списку xs. Використайте функцію foldl.
sumFl :: [Integer] -> Integer
sumFl xs = foldl (+) 0 xs
  
-- 2.	Функція productFr xs, котра вираховує добуток цілих чисел – елементів списку xs. Використайте функцію foldr.
productFr :: [Integer] -> Integer
productFr xs = foldr (*) 1 xs

-- 3.	Функція concatFr xs ys, котра виконує конкатенацію двох списків цілих чисел xs і ys.
--      Функція повинна мати ту  ж семантику, що і оператор (++). Використайте функцію foldr.
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs

-- 4.	Функція sortInsert xs, котра сортує список цілих чисел в порядку зростання.
--      Використайте функцію foldl. Можна побудувати допоміжну функцію insert xs v, 
--      що вставляє новий елемент v у впорядкований список xs. 
sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insert [] xs

insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (y:ys) x = if x < y then x : y : ys else y : insert ys x 

-- 5.	Функція  findIndices p xs, котра  знаходить індекси тих елементів списку цілих чисел xs, 
--      котрі задовольняють предикат p. Індекси елементів починаються з нуля.
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices p xs = [snd x | x <- (createPairs xs 0), p (fst x)] 

createPairs :: [Int] -> Int -> [(Int,Int)]
createPairs xs x= if null xs then [] else (head xs, x) : createPairs (tail xs) (x+1)

-- 6.	Функція  allReverse xss, котра  бере список рядків xss  і перевертає список xss і всі рядки,
--      що входять до нього.  Використайте функції map і reverse.
allReverse :: [String] -> [String]
allReverse xss = reverse (map reverse xss)

-- 7.	Функція noDigits xs, котра вилучає з рядка xs всі десяткові цифри. Використайте функції filter і elem.
noDigits :: String -> String
noDigits xs = filter (\x -> not (checkDigit x)) xs

checkDigit :: Char -> Bool
checkDigit c = elem c ['0' .. '9']

-- 8.	Функція cntGood ps v, котра вираховує скільком предикатам зі списку  ps задовольняє ціле значення v.
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps x = if null ps then 0 else if (head ps x) == True then (cntGood(tail ps) x) + 1 else cntGood(tail ps) x

-- 9.	Функція trianglePas , котра генерує нескінченний  список рядків трикутника Паскаля.
--      Використайте функцію  iterate або scanl.
trianglePas :: [[Integer]]
trianglePas =  scanl (\x y -> y x) [1] (repeat creatingLevel)

creatingLevel:: [Integer] -> [Integer]
creatingLevel n  = zipWith (\x y -> x + y) ([0] ++ n) (n ++ [0])

-- 10.	Функція factorialsM, котра будує нескінченний список факторіалів. Використати функцію zipWith.
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]