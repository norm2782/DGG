module DGG.Parser (
      genCode
    , mkTCI
    ) where

import Data.Generics
import DGG.Adapter
import DGG.Data
import Language.Haskell.Exts

genCode :: ParseResult Module -> LibParser -> LibSupport -> [ImportDecl] -> String
genCode (ParseFailed l m) _ _ _  = error $ "Failed to parse module."
                                        ++ "Error on line " ++ show l ++ ": " ++ m
genCode (ParseOk m)       p s is = prettyPrint (mkModule p is $ listify s m)

-- TODO: This is called a parser, but it also generates code. Naming conflict!
mkModule :: LibParser -> [ImportDecl] -> [Decl] -> Module
mkModule _ _  [] = error "No compatible datatypes found."
mkModule p is xs = Module srcLoc (ModuleName "GenericReps") [] Nothing Nothing is
                       $ concat $ map (p . mkTCI) xs

mkTCI :: Decl -> TCInfo
mkTCI (DataDecl _ _ _ n tv ds _) = TCInfo (fromName n) TyDataType tv $
                                          map mkVCI $ zip [0..] ds
mkTCI _ = error "Only regular datatypes are supported at this moment."

mkVCI :: (Int, QualConDecl) -> VCInfo
mkVCI (i, (QualConDecl _ tvs _ (ConDecl n bts))) =
    VCInfo (fromName n) (length bts) i Nonfix LeftAssoc tvs $ map mkRec bts 

mkRec :: BangType -> Record
mkRec (BangedTy _)     = error "Not supported yet"
mkRec r@(UnBangedTy _) = Record Nothing "someNam" r
mkRec (UnpackedTy _)   = error "Not supported yet"

fromName :: Name -> String
fromName (Ident n)  = n
fromName (Symbol n) = n

