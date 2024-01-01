module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) a b = BinaryTerm Plus a b

infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) a b = BinaryTerm Minus a b

infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) a b = BinaryTerm Times a b

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (BinaryTerm op lhv rhv) = BinaryTerm op (replaceVar varName replacement lhv) (replaceVar varName replacement rhv)
replaceVar varName replacement (IntConstant intValue) = IntConstant intValue
replaceVar varName replacement (Variable originName) | originName == varName = replacement
                                                     | otherwise = Variable originName

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm Times (IntConstant leftConst) (IntConstant rightConst)) = IntConstant $ leftConst * rightConst
evaluate (BinaryTerm Minus (IntConstant leftConst) (IntConstant rightConst)) = IntConstant $ leftConst - rightConst
evaluate (BinaryTerm Plus (IntConstant leftConst) (IntConstant rightConst)) = IntConstant $ leftConst + rightConst
evaluate (BinaryTerm op (BinaryTerm lop llhv lrhv) (BinaryTerm rop rlhv rrhv)) = evaluate $ BinaryTerm op (evaluate $ BinaryTerm lop llhv lrhv) (evaluate $ BinaryTerm rop rlhv rrhv)
evaluate x = x