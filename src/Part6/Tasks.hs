{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map
import Data.List
import qualified Data.Map as Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       toMapForm:: mx -> Map (Int, Int) Int
       toListForm :: mx -> [[Int]]
       getEl ::  Int -> Int -> mx -> Int

       eye :: Int -> mx
       zero :: Int -> Int -> mx
       multiplyMatrix :: mx -> mx -> mx
       multiplyMatrixListInt :: mx -> mx -> [[Int]]
       rows :: mx -> Int
       cols :: mx -> Int

       getEl y x m = findWithDefault 0 (y,x) $ toMapForm m
       multiplyMatrixListInt a b = if cols a == rows b then multiplyM (toMapForm a) (toMapForm b) else error "mult of matrix is impossible"
              where multiplyM x y = [[ sum [ (findWithDefault 0 (l,m) x)*(findWithDefault 0 (m,n) y) | m <- [0..(rows b)-1]] | n <- [0..(cols b)-1] ] | l <- [0..(rows a)-1]]
-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       toMapForm a = Map.insert (0,0) a Data.Map.empty
       toListForm a = [[a]]

       rows _ = 1
       cols _ = 1

       eye _ = 1
       zero _ _ = 0
       multiplyMatrix a b = a * b


instance Matrix [[Int]] where
       toMapForm l = listToMap l 0 0 (Data.Map.empty)
              where 
                     listToMap [[]] h w m = m
                     listToMap ([]:ls) h w m = listToMap ls (h+1) 0 m
                     listToMap (els:ls) h w m = listToMap (tail els:ls) h (w+1) (Map.insert (h,w) (head els) m)
       toListForm l = l

       rows m = length m 
       cols (r:rs) = if rs == [] || length r == cols rs then length r else error ("rows with dif length" ++ (show (r:rs)))
       cols [] = 0

       eye w = [ [ if ww == h then 1 else 0 | ww <- [0..w-1]] | h <- [0..w-1]]
       zero w h = [ [ 0 | ww <- [0..w-1]] | hh <- [0..h-1]]
       multiplyMatrix a b = multiplyMatrixListInt a b

instance Matrix (SparseMatrix Int) where
       toMapForm m = sparseMatrixElements m
       -- toMapForm m = toMap 0 0 (sparseMatrixHeight m) ((sparseMatrixWidth m)-1)  (sparseMatrixElements m)
       --        where 
       --               toMap y x h w m | y >= h = m
       --                               | x == w = toMap (y+1) 0 h w myInsert
       --                               | otherwise = toMap y (x+1) h w myInsert
       --                               where myInsert = if member (y,x) m then m else Map.insert (y,x) 0 m

       toListForm m = [ [ findWithDefault 0 (hh,ww) mm | ww <- [0..w-1]] | hh <- [0..h-1]]
              where w = sparseMatrixWidth m
                    h = sparseMatrixHeight m
                    mm = sparseMatrixElements m
       
       rows m = sparseMatrixHeight m
       cols m = sparseMatrixWidth m

       eye w = SparseMatrix w w (Data.Map.fromList [((i, i), 1) | i <- [0..w-1]])
       zero w h = SparseMatrix w h Data.Map.empty
       multiplyMatrix a b = SparseMatrix (cols b) (rows a) $ toMapForm $ multiplyMatrixListInt a b

-- Реализуйте следующие функции
-- Единичная матрица
-- eye :: Matrix m => Int -> m
-- Матрица, заполненная нулями
-- zero :: Matrix m => Int -> Int -> m
-- zero w h = notImplementedYet
-- Перемножение матриц
-- multiplyMatrix :: Matrix m => m -> m -> m
-- multiplyMatrix = notImplementedYet
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
