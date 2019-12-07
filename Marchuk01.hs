{-# OPTIONS_GHC -Wall #-}
module Marchuk01 where

-- 1.	Функція  power3, котра  будує нескінченний список  третьої степені натуральних чисел.
power3 :: [Integer]
power3 = [x*x*x | x <- [1 .. ]]

-- 2.	Функція  toPower3, котра  будує нескінченний список  натуральних степенів числа три.
toPower3 :: [Integer]
toPower3 = [(3::Integer)^x | x <- [1::Integer .. ]]

-- 3.	Функція  sumPower3 n, котра  обраховує суму ряду for(int i=1; i<=n; i++) res += 3^i
--      Можна скористатися функцією sum xs, що рахує суму всіх елементів списку xs.
sumPower3 :: Integer -> Integer
sumPower3 x = sum (take (fromInteger x) toPower3)

-- 4.	Функція  sumPower  m n, котра  обраховує суму ряду  for(int i=1; i<=n; i++) res += F(m,i)
--      де F(m,i) = m^i Передумова - m≥0. Можна скористатися функцією sum.
sumPower :: Integer -> Integer -> Integer
sumPower x y = if x < 0 then 0 else sum [x^i | i <- [1 .. y]]

-- 5.	Функція lessMe xs, котра знаходить для кожного елементу списку xs, 
--      скільки є елементів строго менших за нього, 
--      і виводить в результуючий список на відповідну позицію цю кількість.
lessMe :: [Int] -> [Int]
lessMe xs = [lessThan x xs | x <- xs]

lessThan :: Int -> [Int] -> Int
lessThan y ys = if null ys then 0 else if y > head ys then (lessThan y (tail ys))+1 else lessThan y (tail ys) 

-- 6.	Функція  frequency xs, що за списком цілих чисел xs, повертає список пар (елемент, частота).
--      Кожна пара визначає елемент із списку xs і кількість (частоту) його входжень в список xs.
--      Можна скористатися функцією length.
frequency :: [Int] -> [(Int,Int)]
frequency xs = deleteDuplicates [(x,elementFrequency x xs) | x <- xs] 

elementFrequency :: Int -> [Int] -> Int
elementFrequency y ys = if null ys then 0 else if y == head ys then (elementFrequency y (tail ys))+1 else elementFrequency y (tail ys) 

deleteDuplicates :: Eq a => [a] -> [a]
deleteDuplicates [] = []
deleteDuplicates (z:zs) = z : deleteDuplicates(filter(/= z) zs)

-- 7.	Функція hailstone n, що формує наступне число, за числом n в списку числа-градини.
hailstone :: Int -> Int
hailstone x = if ((mod x 2) == 0) then (div x 2) else (x*3 + 1)

-- 8.	Функція hailSeq n, що будує список числа-градини, 
--      котрий породжує число n (цей список починається з числа n). 
--      Можна скористатися функцією hailstone.
hailSeq :: Int -> [Int]
hailSeq x = if x == 1 then [1] else x : hailSeq (hailstone x)

-- 9.	Функція allHailSeq, котра будує нескінченний список всіх списків числа-градини,
--      що породжуються натуральними числами 1,2, … Можна скористатися функцією hailSeq.
allHailSeq :: [[Int]]
allHailSeq = [hailSeq x | x <- [1 .. ]]

-- 10.	Функція firstHailSeq l, котра знаходить мінімальне число,
--      що породжує список числа-градини довжини l. Можна скористатися функцією allHailSeq.
firstHailSeq :: Int -> Int
firstHailSeq l = helpingFirstHailSeq l 1

helpingFirstHailSeq :: Int -> Int -> Int
helpingFirstHailSeq l x = if (l == (length (hailSeq x))) then x else helpingFirstHailSeq l (x + 1)
