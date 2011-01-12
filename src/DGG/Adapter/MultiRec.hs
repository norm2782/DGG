module DGG.Adapter.MultiRec (
      makeMultiRec
    , isSuppMultiRec
    , importsMultiRec
    ) where

import DGG.AdapterAbstract

importsMultiRec :: [ImportDecl]
importsMultiRec = [mkImport "Generics.MultiRec"]

makeMultiRec :: CodeGenerator
makeMultiRec = undefined

-- Returns True when the Decl is of the right type and False otherwise. Several
-- types return False at the moment, because they are not supported yet by this
-- library. Support for these types is planned for future increments.
isSuppMultiRec :: Decl -> Bool
isSuppMultiRec (TypeDecl _ _ _ _)          = False
isSuppMultiRec (TypeFamDecl _ _ _ _)       = False
isSuppMultiRec (DataDecl _ _ _ _ _ _ _)    = True
isSuppMultiRec (GDataDecl _ _ _ _ _ _ _ _) = False
isSuppMultiRec (DataFamDecl _ _ _ _ _)     = False
isSuppMultiRec _                           = False

isSuppMultiRec' :: UnivSupp -> Bool
isSuppMultiRec' Regular      = False
isSuppMultiRec' HigherKinded = False
isSuppMultiRec' Nested       = False
isSuppMultiRec' NestedHigherKinded = False
isSuppMultiRec' OtherH98     = False
isSuppMultiRec' SubUniv      = False
-- TODO: Verify truth values
isSuppMultiRec' HigherRankCon = False
isSuppMultiRec' ExistentialTypes = False
isSuppMultiRec' SuppGADTs        = False
isSuppMultiRec' MutRec        = True

