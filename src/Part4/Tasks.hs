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
    where 
        toR rlst [] = rlst
        toR rlst (init : last) =  toR (rlst :< init) last

-- Реализуйте все представленные ниже классы (см. тесты)
instance (Show a) => Show (ReverseList a) where
    show x = "[" <> showEl x <> "]"
        where
            showEl REmpty = ""
            showEl (REmpty :< x) = show x
            showEl (xs :< x) = showEl xs <> "," <> show x
            
instance (Eq a) => Eq (ReverseList a) where
    (==) (xs :< x) (ys :< y) = (x == y) && (xs == ys)
    (==) REmpty REmpty = True
    (==) _ _ = False
    (/=) a b = not $ (==) a b
    
instance Semigroup (ReverseList a) where
    REmpty <> b = b
    a <> REmpty = a
    a <> (bs :< b) = (a <> bs) :< b
    
instance Monoid (ReverseList a) where
    mappend = (<>)
    mempty = REmpty

instance Functor ReverseList where
    fmap f REmpty = REmpty
    fmap f (xs :< x) = (fmap f xs) :< f x

instance Applicative ReverseList where
    pure a = REmpty :< a
    REmpty <*> _ = REmpty
    _ <*> REmpty = REmpty    
    (fs :< f) <*> xs = (fs <*> xs) <> fmap f xs

instance Monad ReverseList where
    return a = REmpty :< a
    REmpty >>= f = REmpty
    (xs:<x) >>= f = (xs >>= f) <> f x
