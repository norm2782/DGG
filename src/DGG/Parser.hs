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

mkModule :: LibParser -> [Decl] -> Module
mkModule p xs = Module (SrcLoc "" 0 0) (ModuleName "") [] Nothing Nothing [] $ map p xs

genCode :: ParseResult Module -> LibParser -> LibSupport -> String
genCode (ParseFailed l m) _ s = error $ "Failed to parse module."
                                     ++ "Error on line " ++ show l ++ ": " ++ m
genCode (ParseOk m)       p s = prettyPrint (mkModule p $ listify s m)

