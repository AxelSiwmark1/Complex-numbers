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

