{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestTH where

import Generics.EMGM
import Data.DeriveTH
import DGG.Adapter.EMGM

data Foo = Bar
data List a = Nil | Cons a (List a)

$(derive deriveEMGM ''Foo)

$(derive deriveEMGM ''List)
