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
        from Ba = L       Unit
        from Ca = R (L    Unit)
        from Da = R (R (L Unit))
        from Ea = R (R (R Unit))
        to (L       Unit)   = Ba
        to (R (L    Unit))  = Ca
        to (R (R (L Unit))) = Da
        to (R (R (R Unit))) = Ea


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
  to' (L a)                   = Leaf    a
  to' (R (L (a :*: b)))       = Branch  a b
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

data A = Ba | Ca | Da | Ea

aTC = TCInfo "A" TyDataType [
    VCInfo "Ba" 0 0 Nonfix M.LeftAssoc [],
    VCInfo "Ca" 0 1 Nonfix M.LeftAssoc [],
    VCInfo "Da" 0 2 Nonfix M.LeftAssoc [],
    VCInfo "Ea" 0 3 Nonfix M.LeftAssoc []]

aList = TCInfo "List" TyDataType [
    VCInfo "Nil"  0 0 Nonfix M.LeftAssoc [],
    VCInfo "List" 2 1 Nonfix M.LeftAssoc [ Record Nothing "a" undefined
                                         , Record Nothing "b" undefined]]


data Comp f g a = C (f (g a))


ggComp = EP from' to' where
    from' (C fga) = fga
    to' fga = C fga

data HFix f a   = Hln (f (HFix f) a)

-- To describe any value constructor in an EP, all that is required are the
-- value constructor name and the number of records to the value constructor.
-- These do need to be given unique names, but their type is not explicitly 
-- refered to in the EP.
--
-- The types are required in order to generate a type signature though.
-- The question is if we want to generate a type signature for the EP. In
-- general, the compiler can infer that by itself. TODO: Ask Sean about cases
-- where the compiler would fail to infer the types.
ggHFix = EP from' to' where
    from' (Hln ffa) = ffa
    to' ffa = Hln ffa
