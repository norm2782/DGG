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
mkTCI (DataDecl _ _ _ n tv ds _) = TCInfo (fromName n) TyDataType
    (parseTyVarBind tv) $ map mkVCI $ zip [0..] ds
mkTCI _ = error "Only regular datatypes are supported at this moment."

parseTyVarBind :: [TyVarBind] -> [TCVar]
parseTyVarBind = foldr parseBind []

parseBind :: TyVarBind -> [TCVar] -> [TCVar]
parseBind (KindedVar n k) ts = TCVar (fromName n) (Just k) : ts
parseBind (UnkindedVar n) ts = TCVar (fromName n) Nothing : ts

mkVCI :: (Int, QualConDecl) -> VCInfo
mkVCI (i, (QualConDecl _ tvs _ (ConDecl n bts))) =
    VCInfo (fromName n) (length bts) i Nonfix LeftAssoc $ map mkBTRec bts
mkVCI (i, (QualConDecl _ tvs _ (InfixConDecl btl n btr))) =
    VCInfo (fromName n) 2 i Nonfix LeftAssoc $ map mkBTRec [btl, btr]
mkVCI (i, (QualConDecl _ tvs _ (RecDecl n bts))) =
    VCInfo (fromName n) (length bts) i Nonfix LeftAssoc $ map mkRec $ fromRec bts

fromRec :: [([Name], BangType)] -> [(String, BangType)]
fromRec []     = []
fromRec (x:xs) = fromNBT x ++ fromRec xs -- TODO: Horribly inefficient!

fromNBT :: ([Name], BangType) -> [(String, BangType)]
fromNBT ([], _)      = []
fromNBT ((x:xs), bt) = (fromName x, bt) : fromNBT (xs, bt)

mkRec :: (String, BangType) -> VCVar
mkRec (_, BangedTy _)       = error "Not supported yet"
mkRec (_, UnpackedTy _)     = error "Not supported yet"
mkRec (n, r@(UnBangedTy t)) = VCVar (Just n) r Nothing -- TODO: Kind info?

-- TODO: Lots
mkBTRec :: BangType -> VCVar
mkBTRec (BangedTy _)     = error "Not supported yet"
mkBTRec r@(UnBangedTy t) = VCVar Nothing r Nothing 
mkBTRec (UnpackedTy _)   = error "Not supported yet"

fromName :: Name -> String
fromName (Ident n)  = n
fromName (Symbol n) = n

