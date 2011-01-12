module DGG.Data where

import Language.Haskell.Exts.Syntax

-- TODO: Define TCInfo instances for primitive types?
-- TODO: Capture quantified types and GADTs
--
-- TODO: In EMGM module: put together the representations as used by EMGM using
-- haskell-src-exts. Then use these with Derive to either generate code or to
-- feed TH.
--
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

data TypeType = TyDataType
              | TyNewType
              | TySynonym
              | TyGADT
              deriving (Show)  

data ConFixity = Nonfix
               | Infix 
               | Infixl
               | Infixr
               deriving (Eq, Show)

data Associativity = LeftAssoc
                   | RightAssoc
                   deriving (Show)

data TCVar = TCVar { tcvName :: Name
                   , tcvKind :: Maybe Kind }
           deriving (Show)

data VCVar = VCVar { vcvRec   :: Maybe Name
                   , vcvBType :: Type }
           deriving (Show)

-- TODO: Do we need tycon arity? or just distill that from list length?
data TCInfo = TCInfo { tcName :: Name 
                     , tcType :: TypeType
                     , tcVars :: [TCVar]
                     , tcVCs  :: [VCInfo]
                     }
            deriving (Show)

-- TODO: Remove Arity here and calculate that based on vcVars
data VCInfo = VCInfo { vcName    :: Name
                     , vcArity   :: Int
                     , vcIndex   :: Int
                     , vcFixity  :: ConFixity
                     , vcAssoc   :: Associativity
                     , vcVars    :: [VCVar]
                     }
            deriving (Show)
