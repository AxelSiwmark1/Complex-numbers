data ImagUnit = IU deriving Show
i :: ImagUnit
i = IU

type REAL = Double
-- a + bi or a + ib
data CB where
    Plus1 :: REAL -> REAL -> ImagUnit -> CB
    Plus2 :: REAL -> ImagUnit -> REAL -> CB
 deriving (Show)
-- Print complex number for ease of use
showCB :: CB -> String
showCB (Plus1 x y i) = show x ++ "+" ++ show y ++ "i"
showCB (Plus2 x i y) = show x ++ "+" ++ "i" ++ show y

-- Semantics side
-- PlusI a b == Plus1 a b i == Plus2 a i b 
data CC where
    PlusI :: REAL -> REAL -> CC   -- Real part + imaginary part
    deriving (Show)


equalityCheck :: CC -> CC -> Bool
equalityCheck (PlusI a b) (PlusI x y) = a==x && b==y

re :: CC -> REAL
re (PlusI x y) = x

im :: CC -> REAL
im (PlusI x y) = y

-- Sum of two complex number
addCC :: CC -> CC -> CC
addCC (PlusI a b) (PlusI x y) = PlusI (a + x) (b + y)

mulCC :: CC -> CC -> CC
mulCC (PlusI a b) (PlusI x y) = PlusI r c where
    r = a*x - b*y
    c = a*y + b*x

-- (a + bi)(x - yi)
--  a*x + b*y
divCC :: CC -> CC -> CC
divCC (PlusI a b) (PlusI x y) = PlusI real img
    where
        real = (a*x - b*y) / (a*x + b*y)
        img = (b - y) / (a*x + b*y)

rconCC :: REAL -> CC
rconCC r = PlusI r 0

iCC :: CC
iCC = PlusI 0 1

-- Syntax side
data CE where
    Add :: CE -> CE -> CE
    Mul :: CE -> CE -> CE
    Div :: CE -> CE -> CE
    RCon :: REAL -> CE  -- REAL constants
    I :: CE
    deriving(Show)

eval :: CE -> CC
eval (Add e1 e2) = addCC (eval e1) (eval e2)
eval (Mul e1 e2) = mulCC (eval e1) (eval e2) 
eval (Div e1 e2) = divCC (eval e1) (eval e2)
eval I = iCC
eval (RCon r) = rconCC r


--Test cases
-- 3 + i
e1 :: CE
e1 = Add (RCon 3) I
-- 5*i
e2 :: CE
e2 = Mul (RCon 5) I