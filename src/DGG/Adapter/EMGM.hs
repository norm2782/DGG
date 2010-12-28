module DGG.Adapter.EMGM (
      makeEMGM
    , deriveEMGM
    , isSuppEMGM
    , importsEMGM
    ) where

import Data.Derive.Internal.Derivation
import DGG.Adapter as D
import DGG.Data as D
import DGG.Parser as D
import Language.Haskell

importsEMGM :: [ImportDecl]
importsEMGM = [mkImport "Generics.EMGM"]

-- TODO: The definitions of deriveEMGM and fddE are quick and dirty
-- implementations to make the types work. They need a lot of work still.
deriveEMGM :: Derivation
deriveEMGM = derivationCustom "myEMGMType" fddE

fddE :: FullDataDecl -> Either String [Decl]
fddE ((ModuleName n), decl) = Right [(makeEMGM . mkTCI) decl]

makeEMGM :: LibParser
makeEMGM tc@(TCInfo _ TyDataType _) = createDTEP   tc
makeEMGM tc@(TCInfo _ TyNewType  _) = createNTEP   tc
makeEMGM tc@(TCInfo _ TySynonym  _) = createSynEP  tc
makeEMGM tc@(TCInfo _ TyGADT     _) = createGADTEP tc

-- Returns True when the Decl is of the right type and False otherwise. Several
-- types return False at the moment, because they are not supported yet by this
-- library. Support for these types is planned for future increments.
-- TODO: Perhaps this check should be performed on the DGG custom datatypes
-- instead? They can tell if a datatype is mutually recursive, etc.
isSuppEMGM :: Decl -> Bool
isSuppEMGM (TypeDecl _ _ _ _)          = False
isSuppEMGM (TypeFamDecl _ _ _ _)       = False
isSuppEMGM (DataDecl _ _ _ _ _ _ _)    = True
isSuppEMGM (GDataDecl _ _ _ _ _ _ _ _) = False
isSuppEMGM (DataFamDecl _ _ _ _ _)     = False
isSuppEMGM _                           = False

createDTEP :: TCInfo -> Decl
createDTEP (TCInfo tn TyDataType vcis) =
  PatBind srcLoc (pVarIdent $ "dggEP_" ++ tn) Nothing rhs (bdecls vcis)

createNTEP   (TCInfo tn TyNewType vcis) = undefined
createSynEP  (TCInfo tn TySynonym vcis) = undefined
createGADTEP (TCInfo tn TyGADT    vcis) = undefined

fromFunName, toFunName, unitType :: String
fromFunName = "from'"
toFunName   = "to'"
unitType    = "Unit"

rhs :: Rhs
rhs = UnGuardedRhs (App (App (Con $ unQualIdent "EP")
                             (mkIdent fromFunName))
                             (mkIdent toFunName))

bdecls :: [VCInfo] -> Binds
bdecls vcis = BDecls $ (map (bdeclFrom ln) vcis) ++ (map (bdeclTo ln) vcis)
    where ln = length vcis

bdeclFrom :: Int -> VCInfo -> Decl
bdeclFrom cnt vci@(VCInfo n a _ _ _ _) =
    FunBind [Match srcLoc (Ident fromFunName) 
        [PApp (unQualIdent n) (map pVarIdent (D.genNames a))]
        Nothing (UnGuardedRhs (fromEP 0 cnt vci)) (BDecls [])]

bdeclTo :: Int -> VCInfo -> Decl
bdeclTo cnt vci =
    FunBind [Match srcLoc (Ident toFunName)
        [toEP 0 cnt vci]
        Nothing (UnGuardedRhs $ mkToRhs vci) (BDecls [])]

mkToRhs :: VCInfo -> Exp
mkToRhs (VCInfo n _ _ _ _ []) = mkCon n
mkToRhs (VCInfo _ a _ _ _ rs) = buildProd a

buildProd :: Int -> Exp
buildProd rs = buildInApp $ reverse (D.genNames rs)

buildInApp :: [String] -> Exp
buildInApp [x]    = mkIdent x
buildInApp (x:xs) = InfixApp (buildInApp xs) expProd (mkIdent x)

expProd :: QOp
expProd = (QConOp . unQualSym) ":*:"

fromEP :: Int -> Int -> VCInfo -> Exp
fromEP _   1  vci = mkFromRs $ conArity vci
fromEP cnt nc vci@(VCInfo _ a i _ _ _)
    | i == cnt + 1 && i == nc - 1 = pAppConUnQualIdent "R" $ mkFromRs a
    | i == cnt  = pAppConUnQualIdent "L" $ mkFromRs a
    | otherwise = App (conUnQualIdent "R") (fromEP (cnt + 1) nc vci)

mkFromRs :: Int -> Exp
mkFromRs 0  = mkCon unitType
mkFromRs rs = buildProd rs

ppPAppConUnQualIdent :: String -> Pat -> Pat
ppPAppConUnQualIdent s e = PApp (unQualIdent s) [e]

mkPRs :: Int -> Pat
mkPRs 0  = (PApp . unQualIdent) unitType []
mkPRs rs = buildPProd rs

buildPProd :: Int -> Pat
buildPProd rs = buildInPApp $ reverse (D.genNames rs)

buildInPApp :: [String] -> Pat
buildInPApp [x]    = mkPIdent x
buildInPApp (x:xs) = PInfixApp (buildInPApp xs) expPProd (mkPIdent x)

expPProd :: QName
expPProd = unQualSym ":*:"

toEP :: Int -> Int -> VCInfo -> Pat
toEP _   1  vci = mkPRs $ conArity vci
toEP cnt nc vci@(VCInfo _ a i _ _ _)
    | i == cnt + 1 && i == nc - 1 = ppPAppConUnQualIdent "R" $ mkPRs a
    | i == cnt  = ppPAppConUnQualIdent "L" $ mkPRs a
    | otherwise = PApp (unQualIdent "R") [(toEP (cnt + 1) nc vci)]
