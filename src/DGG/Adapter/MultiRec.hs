module DGG.Adapter.MultiRec where

import DGG.Data
import Language.Haskell.Exts.Syntax

makeMultiRec :: LibParser
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
