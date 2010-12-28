module DGG.Parser where
{-
module DGG.Parser (
      genCode
    , mkTCI
    ) where
-}
import Data.Generics
import DGG.Adapter
import DGG.Data
import Language.Haskell.Exts

genCode :: ParseResult Module -> LibParser -> LibSupport -> [ImportDecl] -> String
genCode (ParseFailed l m) _ _ _  = error $ "Failed to parse module."
                                        ++ "Error on line " ++ show l ++ ": " ++ m
genCode (ParseOk m)       p s is = prettyPrint (mkModule p is $ listify s m)

-- TODO: This alone probably won't do. The GP libraries in question need to be 
-- imported in the generated module as well. This is adapter-specific and needs
-- to be added on the module level.
-- Also, this is called a parser, but it also generates code. Naming conflict!
mkModule :: LibParser -> [ImportDecl] -> [Decl] -> Module
mkModule _ _  [] = error "No compatible datatypes found."
mkModule p is xs = Module srcLoc (ModuleName "GenericReps") [] Nothing Nothing is
                       $ map (p . mkTCI) xs

mkTCI :: Decl -> TCInfo
mkTCI (DataDecl _ _ _ n _ ds _) = TCInfo (fromName n) TyDataType $ map mkVCI
                                                                 $ zip [0..] ds
mkTCI _ = error "Only regular datatypes are supported at this moment."

mkVCI :: (Int, QualConDecl) -> VCInfo
mkVCI (i, (QualConDecl _ _ _ (ConDecl n bts))) = VCInfo (fromName n) (length bts) i Nonfix
                                           LeftAssoc $ map mkRec bts 

mkRec :: BangType -> Record
mkRec (BangedTy t)   = error "Not supported yet"
mkRec r@(UnBangedTy t) = Record Nothing "someNam" r
mkRec (UnpackedTy t) = error "Not supported yet"

getConName :: ConDecl -> String
getConName (ConDecl n _)        = fromName n
getConName (InfixConDecl _ n _) = fromName n
getConName (RecDecl n _ )       = fromName n

fromName :: Name -> String
fromName (Ident n)  = n
fromName (Symbol n) = n


testMkTCI = mkTCI $ DataDecl (SrcLoc {srcFilename = "TestTH.hs", srcLine = 10, srcColumn = 1}) DataType [] (Ident "List") [UnkindedVar (Ident "a")] [QualConDecl (SrcLoc {srcFilename = "TestTH.hs", srcLine = 10, srcColumn = 15}) [] [] (ConDecl (Ident "Nil") []),QualConDecl (SrcLoc {srcFilename = "TestTH.hs", srcLine = 10, srcColumn = 21}) [] [] (ConDecl (Ident "Cons") [UnBangedTy (TyVar (Ident "a")),UnBangedTy (TyParen (TyApp (TyCon (UnQual (Ident "List"))) (TyVar (Ident "a"))))])] []
