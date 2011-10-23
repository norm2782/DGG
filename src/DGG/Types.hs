module DGG.Types where

data Name
  =  Ident String (Maybe String)
  |  Symbol String String
  deriving (Show)

data DataType
  =  DataType
  {  tyConName  :: Name
  ,  tyKind     :: Kind
  ,  tyVars     :: [String]
  ,  ctors      :: [Con]
  }
  deriving (Show)

data Kind
  =  KindStar
  |  KindFun Kind Kind
  deriving (Show)

data Assoc
  =  AssocNone
  |  AssocLeft
  |  AssocRight
  deriving (Show)

data Fixity
  =  Fixity Assoc Int
  deriving (Show)

data Con = Con
  {  conName    :: Name
  ,  conFixity  :: Fixity
  ,  conFields  :: Fields
  }
  deriving (Show)

data Type
  =  TyVar String
  |  TyCon Name
  |  TyApp Type Type
  |  TyFun Type Type
  |  TyList Type
  |  TyTuple [Type]
  deriving (Show)

data Fields
  =  Fields [Type]
  |  RecordFields [(Name, Type)]
  deriving (Show)


