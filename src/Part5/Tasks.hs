module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f init [] = init 
myFoldl f init (x:xs) = myFoldl f (f init x) xs

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f init [] = init
myFoldr f init (x:xs) = f x $ myFoldr f init xs

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f l = myFoldr (\ a b -> f a : b) [] l

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f l = myFoldr (\ a b -> f a ++ b) [] l

myConcat :: [[a]] -> [a]
myConcat l = myFoldl (\ a b -> a ++ b) [] l

myReverse :: [a] -> [a]
myReverse l = myFoldl (\ a b -> b:a) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\ a b -> if p a then a:b else b) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\ a b -> if p a then (a: fst b, snd b) else (fst b, a:snd b)) ([],[])

