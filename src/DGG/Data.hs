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

data TCVar = TCVar { tcvName :: String
                   , tcvKind :: Maybe Kind }
           deriving (Show)

data VCVar = VCVar { vcvRec   :: Maybe String
                   , vcvBType :: Type }
           deriving (Show)

-- TODO: Do we need tycon arity? or just distill that from list length?
data TCInfo = TCInfo { tcName :: String
                     , tcType :: TypeType
                     , tcVars :: [TCVar]
                     , tcVCs  :: [VCInfo]
                     }
            deriving (Show)

data VCInfo = VCInfo { vcName    :: String
                     , vcArity   :: Int
                     , vcIndex   :: Int
                     , vcFixity  :: ConFixity
                     , vcAssoc   :: Associativity
                     , vcVars    :: [VCVar]
                     }
            deriving (Show)
