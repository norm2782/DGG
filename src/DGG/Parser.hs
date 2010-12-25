module DGG.Parser (
      module DGG.Adapter.EMGM
    , module DGG.Adapter.MultiRec
    , module DGG.Adapter.SYB
    , genCode
    ) where

import Data.Generics
import DGG.Data
import DGG.Adapter.EMGM
import DGG.Adapter.MultiRec
import DGG.Adapter.SYB
import Language.Haskell.Exts

mkSrc :: LibParser -> [Decl] -> Module
mkSrc p xs = Module (SrcLoc "" 0 0) (ModuleName "") [] Nothing Nothing [] $ map p xs

genCode :: ParseResult Module -> LibParser -> String
genCode (ParseFailed l m) _ = error $ "Failed to parse module. Error on line " ++ show l ++ ": " ++ m 
genCode (ParseOk m)       p = prettyPrint (mkSrc p $ listify isSuppDecl m)

-- Returns True when the Decl is of the right type and False otherwise. Several
-- types return False at the moment, because they are not supported yet by this
-- library. Support for these types is planned for future increments.
isSuppDecl :: Decl -> Bool
isSuppDecl (TypeDecl _ _ _ _)           = False
isSuppDecl (TypeFamDecl _ _ _ _)        = False
isSuppDecl (DataDecl _ _ _ _ _ _ _)     = True
isSuppDecl (GDataDecl _ _ _ _ _ _ _ _)  = False
isSuppDecl (DataFamDecl _ _ _ _ _)      = False
isSuppDecl _                            = False
