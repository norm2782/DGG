{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module TestModule where

-- Product
data MyProd = MyProd Bool Int

type MyProdAlias = MyProd

-- Strict product
data MyStrict = MyStrict !Bool !Int

-- Polymorphic
data MyPoly a = MyPoly a

type MyPolyAlias = MyPoly Int

-- Regular datatype
data List a = Nil | Cons a (List a)

-- Mutual recursive datatypes
data MutRecA a = MRANill a | MutRecA (MutRecB a)
data MutRecB b = MRBNill b | MutRecB (MutRecA b)

-- Nested datatype
data Perfect a = Perfect (Perfect (a,a))

-- Existential 
data Exist = forall a. Exist a

-- GADTs
data Expr a where
    I   :: Int  -> Expr Int
    B   :: Bool -> Expr Bool
    Add :: Expr Int -> Expr Int -> Expr Int
    Mul :: Expr Int -> Expr Int -> Expr Int
    Eq  :: Expr Int -> Expr Int -> Expr Bool

-- Newtype
newtype Foo = Foo Int


data Tree       = Empty | Leaf Int | Node Tree Tree Tree
data TTree a    = Tip Int | Branch (TTree a) a (TTree a)
data Toeplitz a = Toeplitz a [(a,a)]
data Comp f g a = C (f (g a))
data HFix f a   = Hln (f (HFix f) a)

