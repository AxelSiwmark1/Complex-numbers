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


-- PlusI a b == Plus1 a b i == Plus2 a i b 
data CC where
    PlusI :: REAL -> REAL -> CC   -- Real part + imaginary part
    deriving (Show)


equalityCheck :: CC -> CC -> Bool
equalityCheck (PlusI a b) (PlusI x y) = a==x && b==y

