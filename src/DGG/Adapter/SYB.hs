module DGG.Adapter.SYB where

import DGG.Data
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

makeSYB :: LibParser
makeSYB = undefined

-- Returns True when the Decl is of the right type and False otherwise. Several
-- types return False at the moment, because they are not supported yet by this
-- library. Support for these types is planned for future increments.
isSuppSYB :: Decl -> Bool
isSuppSYB (TypeDecl _ _ _ _)          = False
isSuppSYB (TypeFamDecl _ _ _ _)       = False
isSuppSYB (DataDecl _ _ _ _ _ _ _)    = True
isSuppSYB (GDataDecl _ _ _ _ _ _ _ _) = False
isSuppSYB (DataFamDecl _ _ _ _ _)     = False
isSuppSYB _                           = False

