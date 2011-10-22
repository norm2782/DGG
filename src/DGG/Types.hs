module DGG.Types where

import HsTypes

type Name = String

data Ctor = Ctor CName Fixity Fields

data Fields = Fields [HsType]
            | RecordFields [(CName, HsType)]

data CName = Ident String (Maybe String)
           | Symbol String String

data DataType = DataType {
     tyConName  :: Name
  ,  tyVars     :: [Name]
  ,  ctors      :: [Ctor]
}

