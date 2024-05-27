{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (unless)
import Data.List (intercalate)
import Data.Group
import Data.GroupExpr (GroupExpr (GroupExpr), simplify)
import Data.Var (Var)
import Instances
import System.Exit (exitFailure)
import Test.QuickCheck

-- (0 баллов) Выполняется ли свойство `prop_minimum`? Проверьте.
-- (0.5 балла) Объясните.
-- minimum возвращает всегда один элемент котрежа, т.к только один и видит
-- кортеж интерпретируется как последовательность из одного
-- элемента, т.е как элемент и его контекст.

prop_minimum = minimum(100, 500) === 500

-- (1 балл) Выпишите аксиомы групп в виде свойств QuickCheck и проверьте, что
-- они выполняются для Вашей реализации `GroupExpr`.
prop_assoc :: GroupExpr Var -> GroupExpr Var -> GroupExpr Var -> Property
prop_assoc a b c = a <> (b <> c) === (a <> b) <> c

prop_neutral :: GroupExpr Var -> Property
prop_neutral a = a <> mempty === a .&&. mempty <> a === a

prop_inverse :: GroupExpr Var -> Property
prop_inverse a = a <> inverse a === mempty .&&. inverse a <> a === mempty

type TestExpr = GroupExpr Var

return []

main = do
  success <- $verboseCheckAll
  unless success exitFailure
