{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestTH where

--import Generics.EMGM
import Data.DeriveTH
--import DGG.Adapter.EMGM
import DGG.Adapter.SYB
import Data.Data
import Data.Typeable

data Foo = Bar
data List a = Nil | Cons a (List a)
data MyTree a = MyLeaf
              | MyBinNode { lTree :: (MyTree a)
                          , bVal  :: a
                          , rTree :: (MyTree a) }
              | MyRTree a [MyTree a]

{-
$(derive deriveEMGM ''Foo)

$(derive deriveEMGM ''List)

$(derive deriveEMGM ''MyTree)
-}

$(derive deriveSYB ''Foo)

$(derive deriveSYB ''List)

$(derive deriveSYB ''MyTree)

