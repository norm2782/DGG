module DGG.Parser (
      mkTCI
    ) where

import DGG.Adapter
import DGG.Data
import Language.Haskell.Exts

mkTCI :: Decl -> TCInfo
mkTCI (DataDecl _ _ _ n tv ds _) = TCInfo n TyDataType
    (parseTyVarBind tv) $ map mkVCI $ zip [0..] ds
mkTCI _ = error "Only regular datatypes are supported at this moment."

parseTyVarBind :: [TyVarBind] -> [TCVar]
parseTyVarBind = foldr parseBind []

parseBind :: TyVarBind -> [TCVar] -> [TCVar]
parseBind (KindedVar n k) ts = TCVar n (Just k) : ts
parseBind (UnkindedVar n) ts = TCVar n Nothing  : ts

-- TODO: Support for infix operators and support for assiciativity.
mkVCI :: (Int, QualConDecl) -> VCInfo
mkVCI (i, (QualConDecl _ tvs _ (ConDecl n bts))) =
    VCInfo n (length bts) i Nonfix LeftAssoc $ map mkBTRec bts
mkVCI (i, (QualConDecl _ tvs _ (InfixConDecl btl n btr))) =
    VCInfo n 2 i Nonfix LeftAssoc $ map mkBTRec [btl, btr]
mkVCI (i, (QualConDecl _ tvs _ (RecDecl n bts))) =
    VCInfo n (length bts) i Nonfix LeftAssoc $ map mkRec $ fromRec bts

fromRec :: [([Name], BangType)] -> [(Name, BangType)]
fromRec = foldr (\x xs -> fromNBT x ++ xs) [] -- TODO: ++ is inefficient!

fromNBT :: ([Name], BangType) -> [(Name, BangType)]
fromNBT ([], _)      = []
fromNBT ((x:xs), bt) = (x, bt) : fromNBT (xs, bt)

mkRec :: (Name, BangType) -> VCVar
mkRec (n, BangedTy t)   = VCVar (Just n) t
mkRec (n, UnpackedTy t) = VCVar (Just n) t
mkRec (n, UnBangedTy t) = VCVar (Just n) t

mkBTRec :: BangType -> VCVar
mkBTRec (BangedTy t)   = VCVar Nothing t
mkBTRec (UnBangedTy t) = VCVar Nothing t
mkBTRec (UnpackedTy t) = VCVar Nothing t

