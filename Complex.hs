data ImagUnit = IU deriving Show
i :: ImagUnit
i = IU


-- a + bi or a + ib
data CB where
    Plus1 :: REAL -> REAL -> ImagUnit -> CB
    Plus2 :: REAL -> ImagUnit -> REAL -> CB
 deriving (Show)