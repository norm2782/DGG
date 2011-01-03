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

type LibParser  = TCInfo -> [Decl]
type LibSupport = Decl -> Bool

type Arity    = Int
type TypeName = String
type ConName  = String
type ConIndex = Int
type Deconstructor = String

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

data ConType = PrefixType
             | RecordType
             | InfixType
             deriving (Show)

data TVar = TVar { tcvName :: String
                 , tcvKind :: Maybe Kind }
          deriving Show

-- TODO: Typename should either be something like "a", or it should be a
-- reference to some primitive or another datatype. Just a string won't do.
data Record = Record { recDec    :: Maybe Deconstructor
                     , recTyname :: TypeName
                     , recBType  :: BangType
                     }
            deriving (Show)

-- TODO: Do we need tycon arity? or just distill that from list length?
data TCInfo = TCInfo { tcName :: TypeName
                     , tcType :: TypeType
                     , tcVars :: [TVar]
                     , tcVCs  :: [VCInfo]
                     }
            deriving (Show)

data VCInfo = VCInfo { conName    :: ConName
                     , conArity   :: Arity
                     , conIndex   :: ConIndex
                     , conFixity  :: ConFixity
                     , conAssoc   :: Associativity
                     , conVars    :: [TVar]
                     , conRecords :: [Record]
                     }
            deriving (Show)
