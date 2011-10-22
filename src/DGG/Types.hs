module DGG.Types where

import GHC
import HsTypes

data DataType = DataType {
     tyCon  :: TyCon
  ,  ctors  :: [Ctor]
}

type Name = String

data TyCon = TyCon {
     tyConName  :: Name
  ,  tyKind     :: Kind
  ,  tyVars     :: [Name]
}

data Ctor = Ctor {
     ctorName    :: CName
  ,  ctorFixity  :: Fixity
  ,  ctorFields  :: Fields
}

data Fields = Fields [HsType]
            | RecordFields [(CName, HsType)]

data CName = Ident String (Maybe String)
           | Symbol String String

