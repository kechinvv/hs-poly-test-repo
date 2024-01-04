module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = 
    toR REmpty lst
    where toR rlst [] = rlst
          toR rlst (init : last) =  toR (rlst :< init) last

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    -- showPrec _ = show  -- `showPrec' is not a (visible) method of class `Show'
    show x = "[" ++ showEl x ++ "]"
        where
            showEl REmpty = ""
            showEl (REmpty :< x) = show x
            showEl (xs :< x) = showEl xs ++ "," ++ show x

instance (Eq a) => Eq (ReverseList a) where
    (==) (xs :< x) (ys :< y) = (x == y) && (xs == ys)
    (==) REmpty REmpty = True
    (==) _ _ = False
    (/=) a b = not $ (==) a b


instance Semigroup (ReverseList a) where
instance Monoid (ReverseList a) where
instance Functor ReverseList where
instance Applicative ReverseList where
instance Monad ReverseList where
