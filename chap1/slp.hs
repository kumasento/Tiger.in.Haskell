module StraightLineInterpreter where

type Id = String

data Binop = Plus | Minus | Times | Div deriving (Show)

-- 3 types of statements
-- 1. CompoundStm:  Stm -> Stm ; Stm
-- 2. AssignStm:    Stm -> id := Exp
-- 3. PrintStm:     print (ExpList)
data Stm = CompoundStm Stm Stm
         | AssignStm Id Exp
         | PrintStm [Exp]
          deriving (Show)

-- 4 types of expression
-- 1. IdExp:    Exp -> id
-- 2. NumExp:   Exp -> num
-- 3. OpExp:    Exp -> Exp Binop Exp
-- 4. EseqExp:  Exp -> (Stm, Exp)
data Exp = IdExp Id
         | NumExp Int
         | OpExp Exp Binop Exp
         | EseqExp Stm Exp
           deriving (Show)

-- question 1: implement maxargs
maxargsExp :: Exp -> Int
maxargsExp (OpExp e1 _ e2) = max (maxargsExp e1) (maxargsExp e2)
maxargsExp (EseqExp s e)   = max (maxargs s) (maxargsExp e)
maxargsExp _               = 0

maxargsExpList :: [Exp] -> Int
maxargsExpList = foldr (max . maxargsExp) 0

maxargs :: Stm -> Int
maxargs (PrintStm es)       = max (length es) (maxargsExpList es)
maxargs (AssignStm _ e)     = maxargsExp e
maxargs (CompoundStm s1 s2) = max (maxargs s1) (maxargs s2)

-- question 2: implement interp
type Table = [(String, Int)]

lookupT :: String -> Table -> Int
lookupT _ []              = -1
lookupT key ((k,v):ts) 
              | k == key  = v
              | otherwise = lookupT key ts

updateT :: String -> Int -> Table -> Table
updateT s v t = t ++ [(s, v)]

interpStm :: Stm -> Table -> Table
interpStm (CompoundStm s1 s2) t = interpStm s2 (interpStm s1 t)
interpStm (AssignStm i e) t     = updateT i v t'
                                  where 
                                    (t', v) = interpExp e t
interpStm (PrintStm _) t        = t

interpExp :: Exp -> Table -> (Table,Int)
interpExp (IdExp i) t           = (t,   lookupT i t)
interpExp (NumExp num) t        = (t,   num)
interpExp (OpExp e1 binop e2) t = (t2,  v)
                                  where 
                                    (t1, v1)  = interpExp e1 t
                                    (t2, v2)  = interpExp e2 t1

                                    v = case binop of Plus  -> v1 + v2
                                                      Minus -> v1 - v2
                                                      Times -> v1 * v2
                                                      Div   -> v1 `div` v2
interpExp (EseqExp s e) t       =  interpExp e (interpStm s t) 

prog :: Stm
prog = CompoundStm 
        (AssignStm "a" (OpExp (NumExp 5) Plus (NumExp 3)))
        (CompoundStm 
          (AssignStm "b" 
                      (EseqExp 
                        (PrintStm [IdExp "a"
                                  ,OpExp (IdExp "a") Minus (NumExp 1)])
                        (OpExp (NumExp 10) Times (IdExp "a"))))
          (PrintStm [IdExp "b"]))



