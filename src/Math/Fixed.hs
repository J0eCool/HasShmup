module Math.Fixed where

data Fixed = Fixed Integer
    deriving (Eq)

fixedDigits = 3
fixedSize = 10 ^ fixedDigits
fixed n = Fixed . round $ n * fromInteger fixedSize

fixedHoist f (Fixed x) = Fixed $ f x

instance Num Fixed where
    Fixed x + Fixed y = Fixed $ x + y
    Fixed x * Fixed y = Fixed $ x * y `div` fixedSize
    fromInteger n = Fixed $ n * fixedSize
    abs = fixedHoist abs
    signum = fixedHoist signum
    negate = fixedHoist negate

instance Show Fixed where
    show (Fixed x) = show (fromInteger x / fromInteger fixedSize)

instance Fractional Fixed where
    Fixed x / Fixed y = Fixed $ (x * fixedSize) `div` y
    fromRational n = fixed n
