module DGG.Types where

data Name
  =  Ident String (Maybe String)
  |  Symbol String String

data DataType = DataType
  {  tyConName  :: Name
  ,  tyKind     :: Kind
  ,  tyVars     :: [String]
  ,  ctors      :: [Con]
  }

data Kind
  =  KindStar
  |  KindFun Kind Kind

data Assoc
  =  AssocNone
  |  AssocLeft
  |  AssocRight

data Fixity = Fixity Assoc Int

data Con = Con
  {  conName    :: Name
  ,  conFixity  :: Fixity
  ,  conFields  :: Fields
  }

data Type
  =  TyVar String
  |  TyCon Name
  |  TyApp Type Type
  |  TyFun Type Type
  |  TyList Type
  |  TyTuple [Type]

data Fields
  =  Fields [Type]
  |  RecordFields [(Name, Type)]


