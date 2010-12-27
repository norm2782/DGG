module DGG.Parser (
      genCode
    , mkTCI
    ) where

import Data.Generics
import DGG.Adapter
import DGG.Data
import Language.Haskell.Exts

genCode :: ParseResult Module -> LibParser -> LibSupport -> String
genCode (ParseFailed l m) _ _ = error $ "Failed to parse module."
                                     ++ "Error on line " ++ show l ++ ": " ++ m
genCode (ParseOk m)       p s = prettyPrint (mkModule p $ listify s m)

mkModule :: LibParser -> [Decl] -> Module
mkModule _ [] = error "No compatible datatypes found."
mkModule p xs = Module srcLoc (ModuleName "GenericReps") [] Nothing Nothing []
                       $ map (p . mkTCI) xs

mkTCI :: Decl -> TCInfo
mkTCI (DataDecl _ _ _ n _ ds _) = TCInfo (fromName n) TyDataType $ map mkVCI
                                                                 $ zip [0..] ds
mkTCI _ = error "Only regular datatypes are supported at this moment."

mkVCI :: (Int, QualConDecl) -> VCInfo
mkVCI (i, (QualConDecl _ ts _ c)) = VCInfo (getConName c) (length ts) i Nonfix
                                           LeftAssoc $ map mkRec ts 

mkRec :: TyVarBind -> Record
mkRec (KindedVar n _) = Record Nothing (fromName n) (UnBangedTy (TyVar (Ident "a")))
mkRec (UnkindedVar n) = Record Nothing (fromName n) (UnBangedTy (TyVar (Ident "a")))

getConName :: ConDecl -> String
getConName (ConDecl n _)        = fromName n
getConName (InfixConDecl _ n _) = fromName n
getConName (RecDecl n _ )       = fromName n

fromName :: Name -> String
fromName (Ident n)  = n
fromName (Symbol n) = n
