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

data TCVar = TCVar { tcvName :: Name -- ^ Type variable name
                   , tcvKind :: Maybe Kind -- ^ (Opt.) Explicit kind annotation
                   }
           deriving (Show)

data DCVar = DCVar { dcvRec  :: Maybe Name -- ^ (Opt.) Record syntax name
                   , dcvType :: Type -- ^ Variable type
                   }
           deriving (Show)

-- TODO: Do we need tycon arity? or just distill that from list length?
data TCInfo = TCInfo { tcName :: Name 
                     , tcType :: TypeType
                     , tcVars :: [TCVar]
                     , tcDCs  :: [DCInfo]
                     }
            deriving (Show)

data DCInfo = DCInfo { dcName    :: Name
                     , dcIndex   :: Int
                     , dcFixity  :: ConFixity
                     , dcAssoc   :: Associativity
                     , dcVars    :: [DCVar]
                     }
            deriving (Show)

dcArity :: DCInfo -> Int
dcArity = length . dcVars
