{-# OPTIONS_GHC -Wall #-}
module Marchuk09 where

import Data.List
-- СЂРѕР·РіР»СЏРґР°С”РјРѕ Р»РёС€Рµ С†С–Р»С– РґР°РЅС–: СЃРєР°Р»СЏСЂРё  С– РјР°СЃРёРІРё  
--------------------------------------------------------------------
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int 
         | Var Id 
         | OpApp Op Exp Exp 
         | Cond Exp Exp Exp 
         | FunApp Id [Exp] 
         deriving (Eq, Show)

data Stmt = Assign Id Exp 
          | AssignA Id Exp Exp 
          | If Exp Stmt Stmt 
          | While Exp Stmt 
          | Call Id [Exp] 
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- С„СѓРЅРєС†С–С— РїРѕРІРµСЂС‚Р°СЋС‚СЊ Р»РёС€Рµ С†С–Р»С– СЃРєР°Р»СЏСЂРЅС– РґР°РЅС–, РЅРµ РІРёРєРѕСЂРёСЃС‚РѕРІСѓСЋС‚СЊ РіР»РѕР±Р°Р»СЊРЅС– РґР°РЅС– (С‡РёСЃС‚С–!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- СЃС‚РµРє РґР°РЅРёС…

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- 1.	Функція updateValue a b abs, котра в списку пар abs знаходить першу пару (a1,b1), 
--     у якої a1 == a,  і замінює цю пару на пару (a,b).  Якщо такої пари немає, то додає пару (a,b) в список.  
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b xs | notElem a (map fst xs) = xs ++ [(a,b)]
updateValue a b xs = updValue a b xs

updValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updValue _ _ [] = []
updValue a b xs | fst (head xs) == a = (a,b):(tail xs)
                | otherwise = (head xs):(updValue a b (tail xs))

-- 2.	Функція updateArray a i v, що повертає масив такий самий як a за винятком його i елементу, 
--     значення якого потрібно зв’язати з v.
updateArray :: Value -> Value -> Value -> Value
updateArray arr i v = (A (updateValue (gI i) (gI v) (gA arr)))

gA :: Value -> [(Int,Int)]
gA (A xs) = xs
gA (I _) = error "not integer"

gI :: Value -> Int
gI (I x) = x
gI (A _) = error "not array"

-- 3.	Функція  applyOp op v1 v2, котра застосовує оператор op до аргументів v1 і  v2. 
--     Для op =  Index («індексування» масиву) результат повинен бути нуль (представляється I 0),
--     якщо немає зв’язування для індексу v2 в масиві v1.
applyOp :: Op -> Value -> Value -> Value 
applyOp Add vl vl2 = I ((gI vl) + (gI vl2))
applyOp Minus vl vl2 = I ((gI vl) - (gI vl2))
applyOp Mul vl vl2 = I ((gI vl) * (gI vl2))
applyOp Less vl vl2 =  I (boolToInt((gI vl) < (gI vl2)))
applyOp Equal vl vl2 = I (boolToInt((gI vl) == (gI vl2)))
applyOp Index vl vl2 | notElem (gI vl2) (map fst (gA vl)) = (I 0) 
                      |otherwise = (getElemA (gA vl) (gI vl2))

getElemA :: [(Int,Int)] -> Int -> Value
getElemA arr i | fst (head arr) == i = I (snd (head arr))
               | otherwise = getElemA (tail arr) i

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- 4.	Взаємно рекурсивні функції  evExp і  evArgst для обчислення виразів. 
--     Функція evExp e dfx st  обчислює значення вираз e для списку функцій визначених користувачем dfx і стану st.
--     evArgs ex dfx st  застосовує функцію evExp  до кожного елементу списку виразів ex, повертаючи список значень. 
--     Функція evExp e dfx st  використовує правила:
--
--	Значення константи (ціле число) (Const c) є (I c).
--------------------
--	Значення змінної отримується з стану st (стан – це стек, що містить список пар (Id,Value) ).
--------------------
--	Значення умовного  виразу - значення одного з  підвиразів, 
--     в залежності від значення першого (умовного) виразу, значення якого (I v) – False (v == 0) або – True (v/= 0).
--------------------
--	Значення бінарної операції – результат applyOp на обчислених аргументах.
--------------------
--	Щоб обчислити  застосування функції  f до списку виразів es потрібно:
--	 Знайти означення  f в списку dfx
--	 Виділити з означення список імен аргументів as і вираз ef - тіло функції.
--	 Використовуючи evArgs, обчислити кожний вираз в es, отримуючи список значень vs.
--	 Зв’язати імена з as зі значеннями з vs, утворюючи новий стан new.
--	 Обрахувати вираз ef в стані new. 

evExp ::  Exp -> [FunDef] -> StateP -> Value 
evExp (Const c) _ _ = I c
evExp (Var v) _ st = snd (head (filter (\x -> v == fst x) st))
evExp (Cond e1 e2 e3) dfx st | intToBool (gI (evExp e1 dfx st)) = evExp e2 dfx st
                             | otherwise = evExp e3 dfx st
evExp (OpApp op e1 e2) dfx st = applyOp op (evExp e1 dfx st) (evExp e2 dfx st) 
evExp (FunApp nm es) dfx st =     let f = findFunDef dfx nm
                                      as = getFunVar f
                                      ef = getFunExp f
                                      vs = evArgs es dfx st
                                      new = chainNames as vs
                                      in evExp ef dfx new

chainNames :: [VarDef] -> [Value] -> StateP
chainNames vars vals | null vars = []
                     | otherwise = (getVarDefId (head vars), head vals):chainNames (tail vars) (tail vals)

getVarDefId :: VarDef -> Id
getVarDefId (Int v) = v
getVarDefId (Arr v) = v

getFunId :: (Id, ([VarDef], Exp)) -> Id
getFunId (nm, (_,_)) = nm

getFunVar :: (Id, ([VarDef], Exp)) -> [VarDef]
getFunVar (_, (var,_)) = var

getFunExp :: (Id, ([VarDef], Exp)) -> Exp
getFunExp (_, (_,ex)) = ex

findFunDef :: [FunDef] -> Id -> (Id, ([VarDef], Exp))
findFunDef [] _ = error "Doesn't exist"
findFunDef fs nm | getFunId (head fs) == nm = head fs
                     | otherwise = findFunDef (tail fs) nm

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]  
evArgs ex dfs st = map (\e -> evExp e dfs st) ex 

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

-- 5.	Рекурсивна функція evStmt  s dfx dpx st, котра виконує один оператор s. 
--     Результат виконання  – це перетворення вхідного стану у вихідний стан. 
--     Параметри функцій –  список функцій визначених користувачем dfx, 
--     список процедур визначених користувачем dpx і вхідний стан st. 
--     Особливості виконання:
--
--	Виконання  присвоювання змінній (Assign) і елементу масиву (AssignA) припускає, 
--     що відповідні змінна (масив) - «відомі» (є в області дії).
--------------------
--	При реалізації виклику процедури потрібно спочатку розширити стан, 
--     додаючи пари (параметр-значення), а по закінченню обчислення процедури вилучити зі стану ці пари. 

evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign i ex) dfx _ st = tmpAssign i (evExp ex dfx st) st
evStmt (AssignA i ex1 ex2) dfx _ st = tmpAssignA i (evExp ex1 dfx st) (evExp ex2 dfx st) st
evStmt (If ex stmt1 stmt2) dfx dpx st | gI(evExp ex dfx st) /= 0 = evStmt stmt1 dfx dpx st
                                   | otherwise = evStmt stmt2 dfx dpx st
evStmt a@(While ex stmt) dfx dpx st | gI(evExp ex dfx st) == 0 = st
                                   | otherwise = evStmt a dfx dpx (evStmt stmt dfx dpx st)   
evStmt (Call i ex) dfx dpx st =  let (vs,stmt) = getProcedure i dpx 
                                     newSt = newState vs ex dfx st 
                                     res = evStmt stmt dfx dpx newSt 
                                 in deleteLocal vs res 
evStmt (Block vs stmt) dfx dpx st = let fs = addLocalVariables vs st
                                        res = evBlock stmt dfx dpx fs
                                    in deleteLocal vs res


getProcedure::Id->[ProcDef]->([VarDef], Stmt)
getProcedure _ [] = error "empty list"
getProcedure i ((d,v):ps)|i==d = v
                     |otherwise = getProcedure i ps 


newState::[VarDef]->[Exp]->[FunDef]->StateP->StateP
newState var ex dfx st = (chainNames var (evArgs ex dfx st)) ++ st


addLocalVariables::[VarDef]->StateP->StateP
addLocalVariables [] st = st
addLocalVariables (v:vs) st = addLocalVariables vs ((initv v):st)


evBlock:: [Stmt] -> [FunDef]->[ProcDef]->StateP->StateP
evBlock [] _ _ st = st
evBlock (s:ts) dfx dpx st = evBlock ts dfx dpx (evStmt s dfx dpx st) 


deleteLocal :: [VarDef]->StateP->StateP
deleteLocal [] st = st
deleteLocal [Arr v] st = popVars v st
deleteLocal [Int v] st = popVars v st
deleteLocal ((Arr v):vs) st = deleteLocal vs (popVars v st)
deleteLocal ((Int v):vs) st = deleteLocal vs (popVars v st)


popVars::Id -> StateP -> StateP
popVars _ [] = []
popVars i ((d,s):st)|i==d = st
                   |otherwise = (d,s):(popVars i st)

tmpAssignA::Id->Value->Value->StateP->StateP
tmpAssignA i (I x) (I y) [] = [(i, A [(x,y)])]
tmpAssignA i x y ((s,v):st)| s==i = (s, updateArray v x y):st
                         | otherwise = (s,v):(tmpAssignA i x y st)
tmpAssignA _ _ _ _ = undefined


tmpAssign::Id->Value->StateP->StateP
tmpAssign d x [] = [(d,x)]
tmpAssign d x ((i,v):st)|i==d = (i,x):st
                     |otherwise = (i,v):(tmpAssign d x st)

-- 6.	Функція iswfExp e ve fe, котра перевіряє контекстні умови виразу s, 
--     використову-ючи типи доступних змінних (середовище змінних ve) і типи визначених функцій (середовище функцій fe),
--     і повертає тип результату виразу Just t, або Nothing.  
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var i) ve _  = varType i ve
iswfExp (OpApp o x e) v f = let a = (iswfExp x v f)
                                b = (iswfExp e v f)
                            in if (a==Nothing||b==Nothing) then Nothing
                               else let aa = case a of
                                             Just d -> d
                                             _ -> undefined
                                        bb = case b of
                                             Just w -> w
                                             _ -> undefined
                                    in iswfOp o [aa, bb]

iswfExp (Cond i t f) ve fe = let a = iswfExp i ve fe
                                 b =  iswfExp t ve fe
                                 c =  iswfExp f ve fe
                             in if (a==Nothing||b==Nothing||c==Nothing) then c else let ff = case a of
                                                                                                  Just v -> v
                                                                                                  _ -> undefined
                                                                                        tr = case b of
                                                                                                  Just w -> w
                                                                                                  _ -> undefined 
                                                                                        fa = case c of
                                                                                                  Just g -> g  
                                                                                                  _ -> undefined
                                                                                    in iswfCond [ff,tr,fa]

iswfExp (FunApp i ex) ve fe  = let et = listTypes ex ve fe
                                   in case et of 
                                      Nothing -> Nothing
                                      Just t ->case (allTypes i fe) of
                                               Just ty -> if t == ty
                                                             then Just It
                                                             else Nothing
                                               Nothing -> Nothing 

listTypes:: [Exp] -> VarEnv -> FunEnv -> Maybe [Type]
listTypes [] _ _ = Just []
listTypes (e:ex) ve fe = case iswfExp e ve fe of
                           Nothing -> Nothing
                           Just a -> case listTypes ex ve fe of
                                          Nothing -> Nothing
                                          Just arr-> Just (a:arr)

allTypes::Id->FunEnv -> Maybe [Type]
allTypes _ [] = Nothing
allTypes i ((d,t):ve)|i==d = Just t
                     |otherwise = allTypes i ve

varType:: Id->VarEnv->Maybe Type
varType _ [] = Nothing
varType i ((d,t):ve)|i==d = Just t
                    |otherwise = varType i ve

-- 7.	Предикат iswfStmt s ve fe pe, що перевіряє контекстні умови оператору s 
--     в середовищі доступних змінних ve, функцій fe і процедур pe.
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign i ex) vs fs _ = case iswfExp ex vs fs of
                                      Nothing->False
                                      Just t -> case varType i vs of
                                                     Nothing -> False
                                                     Just s -> t==s
iswfStmt (AssignA i ex1 ex2) vs fs _ = case varType i vs of
                                                 Nothing->False
                                                 Just zz ->case iswfExp ex1 vs fs of
                                                               Nothing->False
                                                               Just xx ->case iswfExp ex2 vs fs of
                                                                             Nothing->False
                                                                             Just yy -> iswfAssignA[zz,xx,yy]
iswfStmt (If ex stmt1 stmt2) vs fs pr = case iswfExp ex vs fs of
                                    Just It -> iswfStmt stmt1 vs fs pr && iswfStmt stmt2 vs fs pr
                                    _       -> False   
iswfStmt (While ex stmt) vs fs pr = case iswfExp ex vs fs of
                                        Just It -> iswfStmt stmt vs fs pr
                                        _       -> False   
iswfStmt (Call i es) vs fs pr = case allTypes i pr  of
                                      Nothing -> False 
                                      Just tr -> case listTypes es vs fs of
                                                     Nothing -> False
                                                     Just ye -> tr == ye

iswfStmt (Block vars stmt) vs fs pr = evBlockStmt stmt (additionalVars vars vs) fs pr
                                
additionalVars::[VarDef]->VarEnv->VarEnv
additionalVars [] ve = ve
additionalVars ((Arr nm):ds) vs = (nm,At):(additionalVars ds vs)
additionalVars ((Int nm):ds) vs = (nm,It):(additionalVars ds vs)


evBlockStmt :: [Stmt] -> VarEnv -> FunEnv -> ProcEnv -> Bool
evBlockStmt [] _ _ _ = True
evBlockStmt (st:sts) vs fs pr = iswfStmt st vs fs pr && (evBlockStmt sts vs fs pr)

-- 8.	Предикати iswfFunDef df fe і iswfProcDef dp ve fe pe, 
--     шо перевіряють коректність визначення функції df і процедури dp, 
--     використовуючи середовища змінних ve, функцій fe і процедур pe. 
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef (nm, (vs, ex)) fs =   let res1 = case allTypes nm fs of
                                                 Nothing -> False
                                                 Just t -> (map varToType vs == t)
                                     ve = map givetypeToVar vs
                                     res2 = case iswfExp ex ve fs of
                                                 Just It -> True
                                                 _ -> False   
                                 in res1&&res2     

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef (nm, (vs, s)) vvs fs pr = let tmp = additionalVars vs vvs
                                          res1= iswfStmt s tmp fs pr
                                          res2 = case allTypes nm pr of
                                               Nothing -> False
                                               Just typ -> (map varToType vs == typ)
                                      in res1&&res2

varToType::VarDef -> Type
varToType (Arr _)=At
varToType (Int _)=It

givetypeToVar::VarDef->(Id,Type)
givetypeToVar (Int v) = (v, It)
givetypeToVar (Arr v) = (v, At)
-- 9.	Предикат iswfProgram pr, котрий перевіряє контекстні умови програми, формуючи середовища змінних, функцій і процедур.  
iswfProgram :: Program -> Bool
iswfProgram (vrenv, fnenv, prenv) = let vs = map givetypeToVar vrenv 
                                        fs = map createFEnv fnenv
                                        pr = map createPEnv prenv
                                        res1 = and $ map (\s -> iswfFunDef s fs) fnenv
                                        res2 = and $ map (\s -> iswfProcDef s vs fs pr) prenv
                                        res3 = elem ("main",[]) pr
                                        idVars = map fst vs
                                        idFuns = map fst fs
                                        idProcs = map fst pr
                                        ids = idVars ++ idFuns ++ idProcs
                                        res4 = length ids == length (nub ids) 
                                    in res1 && res2 && res3 && res4

createFEnv::FunDef->(Id,[Type])
createFEnv  (nm, (vs, _)) = (nm, map varToType vs)

createPEnv::ProcDef->(Id,[Type])
createPEnv  (nm, (vs, _)) = (nm, map varToType vs)


--- Р”РѕРїРѕРјС–Р¶РЅС– С„СѓРЅРєС†С–С— -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- РџРµСЂРµРґСѓРјРѕРІР°: РџР°СЂР° Р· РєР»СЋС‡РѕРј a С” РІ СЃРїРёСЃРєСѓ РїР°СЂ abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx) 

-- С„РѕСЂРјСѓС” РїРѕС‡Р°С‚РєРѕРІРµ Р·РЅР°С‡РµРЅРЅСЏ Р·РјС–РЅРЅРѕС—
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0) 

-- Р РµР°Р»С–Р·Р°С†С–СЏ РІРёРєРѕРЅР°РЅРЅСЏ РїСЂРѕРіСЂР°РјРё 
evProgram :: Program -> StateP 
evProgram (dvx, dfx, dpx) = 
   let sb = map initv dvx 
       ( _, s) = lookUp "main" dpx      
   in  evStmt s dfx dpx sb   

--  iswfOp o ts - РїРµСЂРµРІС–СЂСЏС” РєРѕСЂРµРєС‚РЅС–СЃС‚СЊ С‚РёРїС–РІ РѕРїРµСЂР°РЅРґС–РІ ts 
--     Р±С–РЅР°СЂРЅРѕС— РѕРїРµСЂР°С†С–С— o С– С„РѕСЂРјСѓС” С‚РёРї СЂРµР·СѓР»СЊС‚Р°С‚Сѓ Just t Р°Р±Рѕ Nothing  
iswfOp :: Op -> [Type] -> Maybe Type 
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - РїРµСЂРµРІС–СЂСЏС” РєРѕСЂРµРєС‚РЅС–СЃС‚СЊ  С‚РёРїС–РІ РѕРїРµСЂР°РЅРґС–РІ ts
--     СѓРјРѕРІРЅРѕРіРѕ РІРёСЂР°Р·Сѓ С– С„РѕСЂРјСѓС” С‚РёРї СЂРµР·СѓР»СЊС‚Р°С‚Сѓ Just t Р°Р±Рѕ Nothing 
iswfCond :: [Type] -> Maybe Type 
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing 

-- iswfAssignA ts РїРµСЂРµРІС–СЂСЏС” РєРѕСЂРµРєС‚РЅС–СЃС‚СЊ  С‚РёРїС–РІ РѕРїРµСЂР°РЅРґС–РІ ts
--   РѕРїРµСЂР°С†С–С— РїСЂРёСЃРІРѕСЋРІР°РЅРЅСЏ Р·РЅР°С‡РµРЅРЅСЏ РµР»РµРјРµРЅС‚Сѓ РјР°СЃРёРІР° 
iswfAssignA :: [Type] -> Bool
iswfAssignA [At,It,It] = True 
iswfAssignA _          = False  

---- Р”Р°РЅС– РґР»СЏ С‚РµСЃС‚СѓРІР°РЅРЅСЏ  -----------------------
-- РЎС‚Р°РЅ РґР»СЏ С‚РµСЃС‚СѓРІР°РЅРЅСЏ
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv 
varEnv = [("x",It), ("y",It), ("a",At)]

-- Р¤СѓРЅРєС†С–СЏ РјР°РєСЃРёРјСѓРј РґРІРѕС… С‡РёСЃРµР» 
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"], 
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")                                                                
           )
         )
-- Р¤СѓРЅРєС†С–СЏ, С‰Рѕ РѕР±С‡РёСЃР»СЋС” С‡РёСЃР»Рѕ Р¤С–Р±РѕРЅР°С‡С‡С–
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"], 
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Р¤СѓРЅРєС†С–СЏ - СЃСѓРјР° РµР»РµРјРµРЅС‚С–РІ РјР°СЃРёРІСѓ 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0)) 
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- РџСЂРёРєР»Р°Рґ РѕРїРµСЂР°С‚РѕСЂСѓ - Р±Р»РѕРєСѓ 
sampleBlock :: Stmt 
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- РџСЂРѕС†РµРґСѓСЂР° - РґРѕРґР°РІР°РЅРЅСЏ РґРІРѕС… С‡РёСЃРµР»...
-- proc gAdd(x,y) gSum = x + y 
gAdd :: ProcDef
gAdd = ("gAdd", 
        ([Int "x", Int "y"], 
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- РџСЂРѕС†РµРґСѓСЂР° - СЃСѓРјР° РµР»РµРјРµРЅС‚С–РІ РјР°СЃРёРІСѓ 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"], 
          Block [Int "i", Int "limit"] 
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block [] 
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv 
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- РџРѕРІРЅС– РїСЂРѕРіСЂР°РјРё
-- gSum; 
-- proc gAdd(x,y) gSum = x + y 
-- proc main() call gAdd(5,10)   
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... } 
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [], 
       [sumA1, 
        ("main",([], sampleBlock))
       ])
