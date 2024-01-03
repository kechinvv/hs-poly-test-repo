module Part3.Tasks where

--import Data.Map as Map
import Data.List (nub, groupBy, group, sort, head, tail, delete)
import Util (notImplementedYet)
import Data.Char(digitToInt)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : ff f (f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq nums = digitToInt $ snd $ foldr (\ pair1 pair2 -> if fst pair1 >= fst pair2 then pair1 else pair2) (head groupedNums) groupedNums
                where groupedNums = (map (\ l -> (length l, head l)) (group (sort (concat (map show nums)))))

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq list = getUniq [] list
getUniq res (x:xs) | elem x res = getUniq res xs
                   | otherwise = getUniq (x:res) xs
getUniq res [] = res

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = map (\x -> (fst . head $ x, map snd x)) $ groupBy (\ a b -> fst a == fst b) $ notOrderSort res [] uniqFres res
             where res = [((f y), y) | y <- l]
                   uniqFres = nub $ map f l

-- сортировка, так как не указано что k Ordered, то не знаю как без костылей все отсортировать
notOrderSort (l:ls) res (fm:fmeans) hold = if fst l == fm 
                                     then notOrderSort ls (res ++ [l]) (fm:fmeans) hold
                                     else notOrderSort ls res (fm:fmeans) hold
notOrderSort [] res fmeans hold = if length fmeans <= 1 
                                  then res 
                                  else notOrderSort hold res (tail fmeans) hold

-- если бы k был ordered....
-- grokBy f l = toList $ createDict f l Map.empty
-- 
-- createDict f [] d = d
-- createDict f (l:ls) d = if (member res d) == True
--                         then createDict f ls (update (\ el -> if length el /= 0 then Just (l:el) else Nothing) res d)
--                         else createDict f ls (insert res [l] d)
--                         where 
--                             res = f l