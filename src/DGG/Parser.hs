module DGG.Parser (
      mkTCI
    ) where

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

mkVCI :: (Int, QualConDecl) -> DCInfo
mkVCI (i, (QualConDecl _ tvs _ (ConDecl n bts))) =
    DCInfo n i Prefix $ map mkBTRec bts
mkVCI (i, (QualConDecl _ tvs _ (InfixConDecl btl n btr))) =
    DCInfo n i Prefix $ map mkBTRec [btl, btr]
mkVCI (i, (QualConDecl _ tvs _ (RecDecl n bts))) =
    DCInfo n i Prefix $ map mkRec $ fromRec bts

fromRec :: [([Name], BangType)] -> [(Name, BangType)]
fromRec = foldr (\x xs -> fromNBT x ++ xs) [] -- TODO: ++ is inefficient!

fromNBT :: ([Name], BangType) -> [(Name, BangType)]
fromNBT ([], _)      = []
fromNBT ((x:xs), bt) = (x, bt) : fromNBT (xs, bt)

mkRec :: (Name, BangType) -> DCVar
mkRec (n, BangedTy t)   = DCVar (Just n) t
mkRec (n, UnpackedTy t) = DCVar (Just n) t
mkRec (n, UnBangedTy t) = DCVar (Just n) t

mkBTRec :: BangType -> DCVar
mkBTRec (BangedTy t)   = DCVar Nothing t
mkBTRec (UnBangedTy t) = DCVar Nothing t
mkBTRec (UnpackedTy t) = DCVar Nothing t

