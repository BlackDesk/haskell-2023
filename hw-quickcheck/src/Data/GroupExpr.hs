{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Data.GroupExpr where

import Data.Group (Group (..))
import GHC.Num.BigNat (bigNatAdd)
import Data.List (singleton)

-- | @GroupLit a@ is a type of literals in arbitrary group with
-- variables ranging in @a@.
--
-- For example, a literal \(x\) would be encoded as @Direct \'x\'@, while a
-- literal \(y^{-1}\) would be encoded as @Invert \'y\'@.
data GroupLit a = Direct !a | Invert !a deriving (Show, Functor)

-- (0 баллов) Реализуйте более естественное и компактное строковое
-- представление для типа `GroupLit` вместо реализации по умолчанию.

instance Show a => Show (GroupLit a) where
  show :: Show a => GroupLit a -> String
  show (Direct a) = show a
  show (Invert a) = show a ++ "'"

instance Eq a => Eq (GroupLit a) where
  (Direct a) == (Direct b) = a == b
  (Invert a) == (Invert b) = a == b
  _ == _ = False

litInverse :: GroupLit a -> GroupLit a
litInverse (Direct a) = Invert a
litInverse (Invert a) = Direct a

-- | @GroupExpr a@ is a type of expressions in arbitrary group with
-- variables ranging in @a@.
--
-- For example, \(x y z y^{-1}\) would be encoded as
-- @GroupExpr [Direct \'x\', Direct \'y\', Direct \'z\', Invert \'y\']@.
newtype GroupExpr a = GroupExpr {getExpr :: [GroupLit a]}
  deriving (Show, Functor)

-- (1 балл) Сделайте `GroupExpr a` представителем класса `Group`.
-- Можно пользоваться дерайвингом; никаких условий на `a` быть не должно.

instance Semigroup (GroupExpr a) where
    (GroupExpr x) <> (GroupExpr y) = GroupExpr (x ++ y)

instance Monoid (GroupExpr a) where
    mempty = GroupExpr []

instance Group (GroupExpr a) where
  inverse (GroupExpr x) = GroupExpr (
        foldr (\head tail -> tail ++ [litInverse head]) [] x
    )

-- (1 балл) Реализуйте вычисление выражения в группе.
-- (0 баллов) Говоря терминами из алгебры, чем является `groupEval v`?

groupEval ::
  Group g =>
  -- | Variable assignment.
  (x -> g) ->
  -- | Expression to evaluate.
  GroupExpr x ->
  -- | Result of evaluation.
  g
groupEval subst (GroupExpr []) = mempty
groupEval subst (GroupExpr (Direct head: tail)) = subst head <> groupEval subst (GroupExpr tail)
groupEval subst (GroupExpr (Invert head: tail)) = inverse (subst head) <> groupEval subst (GroupExpr tail)


-- (1 балл) Сделайте `GroupExpr` представителем класса `Monad`.
-- Подсказка: у (>>=) есть реализация в одну строчку.

instance Applicative GroupExpr where
  pure x = GroupExpr $ singleton $ Direct x

instance Monad GroupExpr where
    (>>=) :: GroupExpr a -> (a -> GroupExpr b) -> GroupExpr b
    expr >>= func = groupEval func expr

-- (2.5 балла) Реализуйте проверку выражений на равенство с точностью до
-- сокращений:
-- `GroupExpr [Direct 'y', Direct 'x', Invert 'x'] == GroupExpr [Direct 'y']`
-- должно возвращать True.

simplifyInternal :: Eq a => [GroupLit a] -> [GroupLit a] -> [GroupLit a]
simplifyInternal [] (rhv_h: rhv_t) = simplifyInternal [rhv_h] rhv_t
simplifyInternal lhv [] = reverse lhv
simplifyInternal lhv@(lhv_h:lhv_t) (rhv_h:rhv_t) 
    | lhv_h == litInverse rhv_h = simplifyInternal [] (reverse lhv_t ++ rhv_t)
    | otherwise = simplifyInternal (rhv_h : lhv) rhv_t

simplify (GroupExpr x) = GroupExpr $ simplifyInternal [] x
instance Eq a => Eq (GroupExpr a) where
    a == b = let compare a b = length (getExpr a) == length (getExpr b) && 
                  all (uncurry (==)) (zip (getExpr a) (getExpr b))
             in compare (simplify a) (simplify b)

-- (1 балл) Реализуйте более естественное и компактное строковое представление
-- для типа `GroupExpr a` вместо реализации по умолчанию.

instance Show a => Show (GroupExpr a) where
  show (GroupExpr []) = "1"
  show (GroupExpr expr) = let showInternal x = case x of
                               [] -> ""
                               (head: tail) -> show head ++ showInternal tail
                          in showInternal expr