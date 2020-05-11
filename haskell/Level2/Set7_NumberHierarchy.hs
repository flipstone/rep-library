{-

These reps will solidify your grasp of how the numeric types in Haskell
related to one another and how to convert between them. There are a lot of
Reps in this Set, but don't be intimidated -- they're all quite small!

- Write out the numeric class hierarchy using class declarations
- Define 42 as an Integer
- Define 42 as a Rational
- Define 42 as a polymorphic Num
- Convert the Integer 42 to a Rational
- Write a function that builds a Num a from an Integer
- Define 42 as a polymorphic Num by adding two values to make 42
- Define 42 as a polymorphic Num my multiplying two values to make 42
- Convert an Integer 42 to Rational
- Covert a Double 0.25 to Rational
- Write a function that converts a Real a to a Rational
- Convert an Int 42 to an Integer
- Write a function that converts an Integral a to an Integer
- Define 42 as a polymorphic Integral by `div`ving two values to mkae 42
- Convert an Int32 42 to an Int64 (using fromIntegral)
- Write your own version of fromIntegral uing toInteger and fromIntegral
- Define 0.25 as a Rational
- Define 0.25 as a Double
- Define 0.25 as a polymorphic Fractional
- Convert a Rational 0.25 to a Double using fromRational
- Write a function that converts a Rational to a Fractional type a
- Define 0.25 as a polymorphic Fractional by dividing two values using (/)
- Convert a Float 0.25 to a Double using realToFrac
- Write your own version of realToFrac using toRational and fromRational
- Round a Double 0.25 to an Integer
- Ceiling a Rational 0.25 to an Int
- Floor a Float 0.25  to an Int32
- Make as alias for round (so you can write out the type signature)
- Make as alias for ceiling (so you can write out the type signature)
- Make as alias for floor (so you can write out the type signature)
- Define (approximately) 0.25 as a polymorphic Floating using sqrt

-}
module Level2.Set7_NumberHierarchy where

import qualified Data.Int as Int

{-
   Numeric Hierarchy

   class (Show a, Eq a) => Num a
   class (Num a, Ord a) => Real a
   class (Real a, Enum a) => Integral a
   class (Num a) => Fractional a
   class (Real a, Fractional a) => RealFrac a
   class (Fractional a) => Floating a
-}

integer42 :: Integer
integer42 = 42

rational42 :: Rational
rational42 = 42

num42 :: Num a => a
num42 = 42

rationalFromInteger :: Rational
rationalFromInteger = fromInteger 42

numFromInteger :: Num a => Integer -> a
numFromInteger = fromInteger

sum42 :: Num a => a
sum42 = 21 + 21

product42 :: Num a => a
product42 = 2 * 21

integerToRational :: Rational
integerToRational = toRational (42 :: Integer)

doubleToRational :: Rational
doubleToRational = toRational (0.25 :: Double)

realToRational :: Real a => a -> Rational
realToRational = toRational

intToInteger :: Integer
intToInteger = toInteger (42 :: Int)

integralToInteger :: Integral a => a -> Integer
integralToInteger = toInteger

div42 :: Integral a => a
div42 = 84 `div` 2

int32ToInt64 :: Int.Int64
int32ToInt64 = fromIntegral (42 :: Int.Int32)

myFromIntegral :: (Integral a, Num b) => a -> b
myFromIntegral = fromInteger . toInteger

rationalFourth :: Rational
rationalFourth = 0.25

doubleFourth :: Double
doubleFourth = 0.25

fractionalFourth :: Fractional a => a
fractionalFourth = 0.25

rationalToDouble :: Double
rationalToDouble = fromRational 0.25

fractionalFromRational :: Fractional a => Rational -> a
fractionalFromRational = fromRational

quotientFourt :: Fractional a => a
quotientFourt = 1 / 4

floatToDouble :: Double
floatToDouble = realToFrac (0.25 :: Float)

myRealToFrac :: (Real a, Fractional b) => a -> b
myRealToFrac = fromRational . toRational

roundDoubleToInteger :: Integer
roundDoubleToInteger = round (0.25 :: Double)

ceilingRationalToInt :: Int
ceilingRationalToInt = ceiling (0.25 :: Rational)

floorFloatToInt32 :: Int.Int32
floorFloatToInt32 = floor (0.25 :: Float)

myRound :: (RealFrac a, Integral b) => a -> b
myRound = round

myCeiling :: (RealFrac a, Integral b) => a -> b
myCeiling = ceiling

myFloor :: (RealFrac a, Integral b) => a -> b
myFloor = floor

squareRootedQuarter :: Floating a => a
squareRootedQuarter = sqrt (0.25 * 0.25)
