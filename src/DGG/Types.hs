module DGG.Types where

data DataType = DataType {
     tyCon  :: TypeCon
  ,  ctors  :: [Con]
}

type TName = String

data Kind
    = KindStar
    | KindFun Kind Kind

data TypeCon = TypeCon {
     tyConName  :: TName
  ,  tyKind     :: Kind
  ,  tyVars     :: [TName]
}

data Assoc
     = AssocNone
     | AssocLeft
     | AssocRight

data Fixity = Fixity Assoc Int

data Con = Con {
     conName    :: CName
  ,  conFixity  :: Fixity
  ,  conFields  :: Fields
}

type Type = String

data Fields = Fields [Type]
            | RecordFields [(CName, Type)]

data CName = Ident String (Maybe String)
           | Symbol String String


