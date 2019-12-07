{-# OPTIONS_GHC -Wall #-}
module Marchuk07 where

import Data.List

data BinTreeM a = EmptyM 
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq) 
-- B-������ ������� t (NodeB kl tl) =>  
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a]  deriving (Show, Eq)
-- ������� �������������� B-������  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

h, d :: BinTreeM a -> Int
hd :: BinTreeM a -> (Int,Int)
hd EmptyM = (0,0)
hd (NodeM _ _ l r) = let (hl,dl) = hd l
                         (hr,dr) = hd r
                      in ((max hl hr)+1, maximum [dl,dr,abs(hl-hr)])
h = fst . hd
d = snd . hd
-- 1.	Предикат isSearch tr, котрий перевіряє чи являється бінарне дерево з повтореннями tr – бінарним деревом пошуку з повтореннями .
isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True;
isSearch (NodeM v k EmptyM EmptyM) = k>0
isSearch (NodeM v k l r) | tmpRight (NodeM v k l r) && tmpLeft (NodeM v k l r) = isSearch l && isSearch r
                         | otherwise = False

tmpRight :: (Ord a) => BinTreeM a -> Bool
tmpRight EmptyM = True
tmpRight (NodeM v k lt rt) = null (filter (v >) (buildLst rt))

tmpLeft :: (Ord a) => BinTreeM a -> Bool
tmpLeft EmptyM = True
tmpLeft (NodeM v k lt rt) = null (filter (v <) (buildLst lt))

buildLst :: (Ord a) => BinTreeM a -> [a]
buildLst EmptyM = []
buildLst (NodeM v k lt rt) = buildLst lt ++ [v] ++ buildLst rt

brValue :: BinTreeM a -> a
brValue (NodeM u _ _ _) = u

-- 2.	Предикат elemSearch tr v, котрий перевіряє чи містить бінарне дерево пошуку з повтореннями tr значення v
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch tr v | brValue tr == v = True
                | otherwise = (elemSearch (lftTree tr) v) || (elemSearch (rgtTree tr) v)

lftTree, rgtTree :: BinTreeM a -> BinTreeM a
lftTree (NodeM _ _ lt _) = lt
rgtTree (NodeM _ _ _ rt) = rt

-- 3.	Функція insSearch  tr v, що вставляє в бінарне дерево пошуку з повтореннями tr нове значення v.
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
insSearch EmptyM v = (NodeM v 1 EmptyM EmptyM)
insSearch (NodeM u n lt rt) v | u == v = (NodeM u (n+1) lt rt)
                              | u < v = (NodeM u n lt (insSearch rt v))
                              | otherwise = (NodeM u n (insSearch lt v) rt)

quant :: (Ord a) => BinTreeM a -> a -> BinTreeM a
quant (NodeM v num tl tr) tmp | tmp == v = (NodeM v (num + 1) tl tr)
                              | tmp > v = quant tr tmp
                              | otherwise = quant tl tmp

-- 4.	Функція delSearch  tr v, що вилучає з бінарного дерева пошуку tr  значення v. 
--    Якщо дерево не містить значення, то залишається без змін.
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a 
delSearch (NodeM v n EmptyM EmptyM) u | (v == u) && (n > 1) = (NodeM v (n - 1) EmptyM EmptyM)
delSearch (NodeM v _ EmptyM EmptyM) u | v == u = EmptyM
delSearch tr v = bldTrWoutVl EmptyM (doDelete tr v)

bldTrWoutVl :: (Ord a) => BinTreeM a -> [(a,Int)] -> BinTreeM a 
bldTrWoutVl tr lst|null lst = tr
                  |otherwise = bldTrWoutVl (insSearch tr (fst(head lst))) (deleteValue lst (fst(head lst)) (length lst))

doDelete :: (Ord a) => BinTreeM a -> a -> [(a,Int)]
doDelete tr v = deleteValue (listOfNodes tr) v (length (listOfNodes tr))

deleteValue :: (Ord a) => [(a,Int)] -> a -> Int -> [(a,Int)]
deleteValue tr v n|n==0 = tr 
                  |fst(head tr)==v&&snd(head tr)>1 = deleteValue ((tail tr) ++ [(v,(snd(head tr))-1)]) v (n-1)
                  |fst(head tr)==v = deleteValue (tail tr) v (n-1)
                  |otherwise = deleteValue ((tail tr) ++ [head tr]) v (n-1)

listOfNodes :: (Ord a) => BinTreeM a -> [(a,Int)]
listOfNodes tr | tr == EmptyM = []
               | otherwise = [(brValue tr, rptNum tr)] ++ listOfNodes (rgtTree tr) ++ listOfNodes (lftTree tr)

rptNum :: BinTreeM a -> Int
rptNum (NodeM _ n _ _ ) = n 

-- 5.	Функція sortList l, котра сортує список l, використовуючи бінарне дерево пошуку з повтореннями:
-- 	додаючи елементи списку l, формується бінарне дерево пошуку з повтореннями, 
--    починаючи з порожнього, (можна  використати функцію foldl)
-- 	генерується впорядкований список – результат проходження бінарного дерева пошуку з повтореннями в симетричному (централізованому) порядку.

sortList :: (Ord a) => [a] -> [a]
sortList lst = tmpSortList (tmp lst EmptyM)

tmp ::(Ord a) => [a] -> BinTreeM a -> BinTreeM a
tmp lst tr| null lst = tr
          | otherwise = tmp (tail lst) (insSearch tr (head lst))

tmpSortList :: (Ord a) => BinTreeM a -> [a]
tmpSortList EmptyM = []
tmpSortList (NodeM v n lt rt) = tmpSortList lt ++ [v |x <- [1 .. n]] ++ tmpSortList rt

-- 6.	findBInform tr – знаходить для даного tr типу Btree a його характеристики – дане типу BInform a. 
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform btr = BInform {hB = heightB btr, minB = minimumB btr, maxB = maximumB btr}

heightB :: (Bounded a, Ord a) => Btree a -> Int
heightB (NodeB _ []) = 0
heightB (NodeB v btr) = heightB (head btr) + 1

minimumB :: (Bounded a, Ord a) => Btree a  -> a
minimumB (NodeB v []) = head v
minimumB (NodeB v btr) = minimumB (head btr)

maximumB :: (Bounded a, Ord a) => Btree a  -> a
maximumB (NodeB v []) = last v
maximumB (NodeB v btr) = maximumB (last btr)


-- 7.	isBtree t tr  - предикат,  котрий перевіряє чи являється об`єкт  tr типа  Btree  B-де-ревом  порядка t. 
isBtree :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtree t bt =  checkNodes bt
                && checkVal t bt
                && rightOrd bt
                && firstCheck bt
                && specCheck t bt
                && valuesSorted bt

getBValue :: (Bounded a, Ord a) => Btree a -> [a]
getBValue (NodeB key _) = key

isSorted :: (Ord a) => [a] -> Bool
isSorted []       = True
isSorted [_]      = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

rightOrd, checkNodes, valuesSorted, firstCheck :: (Bounded a, Ord a) => Btree a -> Bool 
rightOrd (NodeB _ []) = True
rightOrd (NodeB [] _) = True
rightOrd (NodeB key list) = rightOrd (NodeB (tail key) (tail list))
                              && and[rightOrd el| el<-list]
                              && and[x < head key | x<-getBValue (head list)]
                              && and[x > head key | x<-getBValue (last list)]

checkNodes (NodeB _ []) = True
checkNodes (NodeB key list) = (and [checkNodes el| el<-list])
                             && (((length key) + 1)==length list)

valuesSorted (NodeB _ []) = True
valuesSorted (NodeB key list) = isSorted (concat [getBValue el|el<-list])
                               && isSorted key
                               && and [valuesSorted el| el<-list]

firstCheck bt = length (nub (listOfHsts bt)) == 1

listOfHsts:: (Bounded a, Ord a) => Btree a ->[Int]
listOfHsts (NodeB _ []) = []
listOfHsts (NodeB key list) = listOfHsts (NodeB key (tail list)) ++ [heightB (NodeB key list)]

specCheck, checkVal :: (Bounded a, Ord a) => Int -> Btree a -> Bool
specCheck t (NodeB key []) = (length key >= 1) && (length key <=(2*t-1))
specCheck t (NodeB key list) = (length list == (length key + 1))
                               && (length key >= 1)
                               && (length key <=(2*t-1))

checkVal _ (NodeB _ []) = True
checkVal t (NodeB key list) = (length (getBValue (head list))<=(2*t-1))
                               && (length (getBValue (head list))>=t-1)
                               && checkVal t (head list)
                               && checkVal t (NodeB key (tail list))

-- 8.	Предикат eqBtree tr1 tr2, котрий перевіряє чи являються два B-дерева tr1 і tr2 еквівалентними. 
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool 
eqBtree n bt1 bt2 = (sort (listFromBTree bt1)) == (sort (listFromBTree bt2))

listFromBTree :: (Ord a) => Btree a -> [a]
listFromBTree (NodeB v []) = v
listFromBTree (NodeB v btr) = listFromBTree (NodeB v (tail btr)) ++ listFromBTree (head btr)

-- 9.	Предикат elemBtree tr v, котрий перевіряє чи містить B-дерево tr ключ v.
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr v = elem v (listFromBTree tr) 

position :: Ord a => a -> [a] -> Int
--position v lst = tmpPos v lst ((length lst) - 2)
position s [] = 0
position s (hd:tl) = if s <= hd then 0 else 1 + position s tl

tmpPos :: Ord a => a -> [a] -> Int -> Int
tmpPos v lst i | v <= lst!!i = i
               | otherwise = tmpPos v lst (i+1)

-- 10.	insBtree  t tr v – функція вставки елемента v в B-дерево tr порядка t. 
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree t tr v | (isFull t tr) = insNodeB t (NodeB [k] [tr1, tr2]) v
                | otherwise = insNodeB t tr v
                  where (tr1, k, tr2) = splitAtB t tr

isFull :: Ord a => Int -> Btree a -> Bool
isFull t (NodeB v _) = length v == t*2-1 

insertKey :: Ord a => a -> [a] -> [a]
insertKey v [] = [v]
insertKey v lst | v > head lst = (head lst) : (insertKey v (tail lst)) 
                | otherwise = v : lst
         

insNodeB :: Ord a => Int -> Btree a -> a -> Btree a
insNodeB _ (NodeB v []) ch = NodeB (insertKey ch v) []
insNodeB i (NodeB v btr) ch | (isFull i res4) = NodeB (res1 ++ (k : res2)) (res3 ++ (btr1 : (btr2 : res5)))
                           | otherwise = NodeB v (res3 ++ ((insNodeB i res4 ch) : res5))       
                           where (res1,res2,res3,res4,res5) = decomposeNodeB ch v btr
                                 (tr1, k, tr2) = splitAtB i res4
                                 btr1 = if ch <= k then (insNodeB i tr1 ch) else tr1
                                 btr2 = if ch <= k then tr2 else (insNodeB i tr2 ch)
                            

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] -> ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB v k1 t1 = let index = position v k1
                             t11 = [t1!!i | i <- [0..index-1]]
                             bt = t1!!index
                             t12 = [t1!!i | i <- [index+1..(length t1)-1]]
                             k11 = [k1!!i | i <- [0..index-1]]
                             k12 = [k1!!i | i <- [index..(length k1)-1]]
                           in (k11,k12,t11,bt,t12)

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB t (NodeB k1 t1) = 
    let k11 = [k1!!i | i <- [0..t-2]]
        k12 = [k1!!i | i <- [t..2*t-2]]
        t11 = if (t1==[]) then [] else [t1!!i | i <- [0..t-1]]
        t12 = if (t1==[]) then [] else [t1!!i | i <- [t..2*t-1]]
    in (NodeB k11 t11, k1!!(t-1), NodeB k12 t12)

--------------------- ������ ���� - ������ ������ -------
bm :: BinTreeM Char
bm = NodeM  't' 2  
            (NodeM 'a' 1  EmptyM 
                    (NodeM 'e' 1 
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            ) 
            (NodeM 'w' 2  EmptyM EmptyM)   
{-
tiB1 :: Btree Char 
tiB1 = NodeB ['G','M','P','X'] 
             [ NodeB ['A','C','D','E'] []
             , NodeB ['J','K'] []
             , NodeB ['N','O'] []
             , NodeB ['R','S','T','U','V'] []
             , NodeB ['Y','Z'] [] ]

tBtr1 :: Btree Int
tBtr1 = NodeB [5,10,12] [ts0,ts1,ts2,ts3]
   where ts0 = NodeB [1,3  ] []   --- ,4,5] []  --
         ts1 = NodeB [6,6 ,8,9,10] [] --- ,8,9,10] []  -- ] []   
         ts2 = NodeB [11,11,12,12] []
         ts3 = NodeB [16,16] [] -- ,18,19,20] [] 

tBtr2 :: Btree Int 
tBtr2 = NodeB [15] [ts10,ts11]
  where ts10 = NodeB [11,13] [] 
        ts11 = NodeB [21,22] []  
-}
tBt1 :: Btree Char 
tBt1 = NodeB "L"
       [ NodeB "DG" 
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU" 
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char 
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char 
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char 
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char 
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char 
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char 
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX" 
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]