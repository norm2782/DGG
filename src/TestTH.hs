{-# LANGUAGE TemplateHaskell #-}

module TestTH where

import Generics.EMGM
import Data.DeriveTH
import DGG.Adapter.EMGM

data Foo = Bar

$(derive deriveEMGM ''Foo)
