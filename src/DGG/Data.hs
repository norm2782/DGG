module DGG.Data where

import Language.Haskell.Exts.Syntax

--
-- Datatypes: DataDecl DatatType
-- Type aliases: TypeDecl nothing (just TypeDecl)
-- Newtypes: DataDecl NewType
--

type CodeGenerator = TCInfo -> [Decl]
type LibSupport    = Decl -> Bool

data UnivSupp = Regular
              | HigherKinded
              | Nested
              | NestedHigherKinded
              | OtherH98
              | SubUniv
              | HigherRankCon
              | ExistentialTypes
              | SuppGADTs
              | MutRec
              deriving (Show)

-- Needs to go
data TypeType = TyDataType
              | TyNewType
              | TySynonym
              | TyGADT
              deriving (Show)  

-- TODO: Data types only contain possible infix constructors. InfixDecls need
-- to be parsed separately to provide the right fixity information.
data Fixity = Prefix
            | Infix  -- Int
            | Infixl -- Int
            | Infixr -- Int
            deriving (Eq, Show)

-- | Contains type constructor variable information
data TCVar = TCVar { tcvName :: Name -- ^ Type variable name
                   , tcvKind :: Maybe Kind -- ^ (Opt.) Explicit kind annotation
                   }
           deriving (Show)

-- | Contains data constructor variable information
data DCVar = DCVar { dcvRec  :: Maybe Name -- ^ (Opt.) Record syntax name
                   , dcvType :: Type -- ^ Variable type
                   }
           deriving (Show)

-- | Contains type constructor information
data TCInfo = TCInfo { tcName :: Name 
                     , tcType :: TypeType
                     , tcVars :: [TCVar]
                     , tcDCs  :: [DCInfo]
                     }
            deriving (Show)

-- | Contains data constructor information
data DCInfo = DCInfo { dcName   :: Name
                     {- , dcIndex  :: Int-}
                     , dcFixity :: ConFixity
                     , dcVars   :: [DCVar]
                     }
            deriving (Show)


data DataType = DataType {
     tyCon  :: TyCon
  ,  ctors  :: [Ctor]
}

data CName  =  Ident String
            |  Symbol String String

data Con = Con CName Fixity Fields

data Fields  =  Fields [Type]
             |  RecordFields [(CName, Type)]

-- | Calculate the arity of a data constructor by counting the number of
-- data constructor variables.
dcArity :: DCInfo -> Int
dcArity = length . dcVars
