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

genCode :: ParseResult Module -> LibParser -> LibSupport -> String
genCode (ParseFailed l m) _ s = error $ "Failed to parse module."
                                     ++ "Error on line " ++ show l ++ ": " ++ m
genCode (ParseOk m)       p s = prettyPrint (mkModule p $ listify s m)

mkModule :: LibParser -> [Decl] -> Module
mkModule _ [] = error "No compatible datatypes found."
mkModule p xs = Module (SrcLoc "" 0 0) (ModuleName "") [] Nothing Nothing []
                       $ map (p . mkTCI) xs

mkTCI :: Decl -> TCInfo
mkTCI d = undefined
