module DGG.Types where

-- GHC related imports
import GHC hiding (Kind, Fixity)

data DataType name = DataType {
     tyCon  :: TypeCon
  ,  ctors  :: [Ctor name]
}

type TName = String

-- Stolen from HSE
data Kind
    = KindStar
    | KindBang
    | KindFn Kind Kind
    | KindParen Kind
    | KindVar TName

data TypeCon = TypeCon {
     tyConName  :: TName
  ,  tyKind     :: Kind
  ,  tyVars     :: [TName]
}

-- Stolen from HSE
data Assoc
     = AssocNone
     | AssocLeft
     | AssocRight

-- Stolen from HSE
data Fixity = Fixity Assoc Int

data Ctor name = Ctor {
     ctorName    :: CName
  ,  ctorFixity  :: Fixity
  ,  ctorFields  :: Fields name
}

data Fields name = Fields [HsType name]
                 | RecordFields [(CName, HsType name)]

data CName = Ident String (Maybe String)
           | Symbol String String


