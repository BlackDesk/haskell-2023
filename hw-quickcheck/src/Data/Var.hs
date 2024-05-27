module Data.Var where

-- | A simple variable set.
newtype Var = Var Char deriving (Eq)

instance Show Var where
  show (Var x) = pure x

-- Here was Vasya
