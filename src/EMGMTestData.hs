{-# LANGUAGE TypeOperators #-}

module EMGMTestData where

import EMGMAdapter
import Language.Haskell.Exts.Syntax
import Generics.EMGM as E
import Prelude as P
import Main as M
import Language.Haskell.Exts.Pretty

-- Test code:
--
data Tree a = Leaf a | Branch (Tree a) (Tree a) | LBranch (Tree a) (Tree a) (Tree a)
data List a = Nil | List a (List a)
data Foo = Bar | Baz | Bat

data Foo' = Bar'

ggEPA = EP from to
    where
        from B = L       Unit
        from C = R (L    Unit)
        from D = R (R (L Unit))
        from E = R (R (R Unit))
        to (L       Unit)   = B
        to (R (L    Unit))  = C
        to (R (R (L Unit))) = D
        to (R (R (R Unit))) = E


ggFoo' :: EP Foo' Unit
ggFoo' = EP from' to'
    where
        from' Bar' = Unit
        to' Unit = Bar'

ggEPTree :: EP (Tree t) (t :+: Tree t :*: Tree t
                           :+: Tree t :*: Tree t :*: Tree t)
ggEPTree = EP from' to' where
  from' (Leaf    a)     = L     a
  from' (Branch  a b)   = R (L (a :*: b))
  from' (LBranch a b c) = R (R (a :*: b :*: c))
  to' (L a)                   = Leaf a
  to' (R (L (a :*: b)))       = Branch a b
  to' (R (R (a :*: b :*: c))) = LBranch a b c

ggFoo :: EP Foo (Unit :+: (Unit :+: Unit))
ggFoo = EP from' to'
    where
        from' Bar = L    Unit
        from' Baz = R (L Unit)
        from' Bat = R (R Unit)
        to' (L    Unit)  = Bar
        to' (R (L Unit)) = Baz
        to' (R (R Unit)) = Bat

data A = B | C | D | E

aTC = TCInfo "A" TyDataType [
    VCInfo "B" 0 0 Nonfix M.LeftAssoc [],
    VCInfo "C" 0 1 Nonfix M.LeftAssoc [],
    VCInfo "D" 0 2 Nonfix M.LeftAssoc [],
    VCInfo "E" 0 3 Nonfix M.LeftAssoc []]

aList = TCInfo "List" TyDataType [
    VCInfo "Nil"  0 0 Nonfix M.LeftAssoc [],
    VCInfo "List" 2 1 Nonfix M.LeftAssoc [Record Nothing "a", Record Nothing "b"]]

