module Part1.Tasks where

import Util(notImplementedYet)
import Debug.Trace

to2Pi x = if x >= 2*pi then to2Pi (x - 2*pi) else if x <= -2*pi then to2Pi (x + 2*pi) else x

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = getSin x' x' (x'*x'*x') 1 6 0.0000001 x'
            where x' = to2Pi x

-- getSin res s nx k f e x | trace ("getSin res=" ++ show res ++ " x=" ++ show x ++ " nx=" ++ show nx ++ " s=" ++ show s ++ " k=" ++ show k ++ " f=" ++ show f) False = undefined

getSin res s nx k f e x = if abs s < e then res else getSin (res + ns) ns (nx*x*x) nk (f*(2*nk)*(2*nk+1)) e x
        where ns = ((fromIntegral (myPow (-1) k) ) * nx) / (fromIntegral f)
              nk = k + 1
              

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = getCos 1 1 (x'*x') 1 2 0.0000001 x'
            where x' = to2Pi x

getCos res s nx k f e x = if abs s < e then res else getCos (res + ns) ns (nx*x*x) nk (f*(2*nk)*(2*nk-1)) e x
        where ns = ((fromIntegral (myPow (-1) k) ) * nx) / (fromIntegral f)
              nk = k + 1

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0  =  abs a
myGCD a b  =  myGCD (abs b) (abs (a `rem` b))

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d 02 y = if (mod y 400 == 0  || mod y 100 /= 0 && mod y 4 == 0)
                            then d > 0 && d < 30
                            else d > 0 && d < 29

isDateCorrect d m y = if (elem m [1, 3, 5, 7, 8, 10, 12]) 
                        then d > 0 && d < 32 
                        else if (elem m [2, 4, 6, 9, 11])              
                        then d > 0 && d < 31
                        else False 

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer

myPow x 0 = 1
myPow x y = x * myPow x (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime x = isPrimeIter 2 x (div x 2 + 1)

isPrimeIter i x e = if (i == e) then True else if (mod x i == 0) then False else isPrimeIter (i+1) x e 

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea ((x, y):points) = 1/2 * abs (shapeSum x y 0 points x y)

shapeSum x y res ((cx, cy) :[]) x1 y1 = res + x*cy - cx*y - x1*cy + y1*cx
shapeSum x y res ((cx, cy) : points) x1 y1 = shapeSum cx cy (res + x*cy - cx*y) points x1 y1

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = if (a + b < c || a + c < b || b + c < a) then -1 else getTriangleKind ((a^2 + b^2 - c^2)/(2*a*b))  ((a^2 + c^2 - b^2)/(2*a*c))  ((b^2 + c^2 - a^2)/(2*c*b))
getTriangleKind a b c = if (a == 0 || b == 0 || c == 0) then 2 else if (a < 0 || b < 0 || c < 0) then 0 else 1
