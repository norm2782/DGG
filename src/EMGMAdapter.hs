module EMGMAdapter where


import Prelude as P
import Main as M
import Generics.EMGM as E
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

newPat :: TCInfo -> Decl
newPat tc@(TCInfo _ TyDataType _) = createDTEP   tc
newPat tc@(TCInfo _ TyNewType  _) = createNTEP   tc
newPat tc@(TCInfo _ TySynonym  _) = createSynEP  tc
newPat tc@(TCInfo _ TyGADT     _) = createGADTEP tc

srcLoc = SrcLoc "" 0 0

createDTEP (TCInfo tn TyDataType vcis) =
  PatBind srcLoc (pVarIdent $ "dgg_" ++ tn) Nothing rhs (bdecls vcis)

createNTEP   (TCInfo tn TyNewType vcis) = undefined
createSynEP  (TCInfo tn TySynonym vcis) = undefined
createGADTEP (TCInfo tn TyGADT    vcis) = undefined

fromFunName = "from'"
toFunName   = "to'"
unitType    = "Unit"

rhs = UnGuardedRhs (App (App (Con $ unQualIdent "EP")
                             (mkIdent fromFunName))
                             (mkIdent toFunName))

bdecls :: [VCInfo] -> Binds
bdecls vcis = BDecls $ (P.map (bdeclFrom ln) vcis) ++ (P.map (bdeclTo ln) vcis)
    where ln = length vcis

bdeclFrom :: Int -> VCInfo -> Decl
bdeclFrom cnt vci@(VCInfo n _ _ _ _ rs) =
    FunBind [Match srcLoc (Ident fromFunName) 
        [PApp (unQualIdent n) (P.map (pVarIdent . recTyname) rs)]
        Nothing (UnGuardedRhs (fromEP 0 cnt vci)) (BDecls [])]

bdeclTo :: Int -> VCInfo -> Decl
bdeclTo cnt vci@(VCInfo n ar i f as rs) =
    FunBind [Match srcLoc (Ident toFunName)
        [toEP 0 cnt vci]
        Nothing (UnGuardedRhs $ mkToRhs vci) (BDecls [])]

mkToRhs :: VCInfo -> Exp
mkToRhs (VCInfo n _ _ _ _ []) = Con (UnQual (Ident n))
mkToRhs (VCInfo n _ _ _ _ rs) = buildProd rs

buildProd :: [Record] -> Exp       
buildProd rs = buildInApp $ reverse (P.map recTyname rs)

buildInApp :: [String] -> Exp
buildInApp [x]    = mkIdent x
buildInApp (x:xs) = InfixApp (buildInApp xs) expProd (mkIdent x)

expProd :: QOp
expProd = (QConOp . UnQual . Symbol) ":*:"

mkIdent :: String -> Exp
mkIdent = Var . unQualIdent 

unQualIdent :: String -> QName
unQualIdent = UnQual . Ident

conUnQualIdent :: String -> Exp
conUnQualIdent = Con . unQualIdent

pVarIdent :: String -> Pat
pVarIdent = PVar . Ident

pAppConUnQualIdent :: String -> Exp -> Exp
pAppConUnQualIdent s e = Paren (App (conUnQualIdent s) e)

fromEP :: Int -> Int -> VCInfo -> Exp
fromEP _   1  vci = mkFromRs $ conRecords vci
fromEP cnt nc vci@(VCInfo _ _ i _ _ rs)
    | i == cnt + 1 && i == nc - 1 = pAppConUnQualIdent "R" $ mkFromRs rs
    | i == cnt  = pAppConUnQualIdent "L" $ mkFromRs rs
    | otherwise = App (conUnQualIdent "R") (fromEP (cnt + 1) nc vci)

mkFromRs :: [Record] -> Exp
mkFromRs [] = mkIdent unitType
mkFromRs rs = buildProd rs

ppPAppConUnQualIdent :: String -> Pat -> Pat
ppPAppConUnQualIdent s e = PParen (PApp (unQualIdent s) [e])

mkPIdent :: String -> Pat
mkPIdent = PVar . Ident 

mkPRs :: [Record] -> Pat
mkPRs [] = mkPIdent unitType
mkPRs rs = buildPProd rs

buildPProd :: [Record] -> Pat       
buildPProd rs = buildInPApp $ reverse (P.map recTyname rs)

buildInPApp :: [String] -> Pat
buildInPApp [x]    = mkPIdent x
buildInPApp (x:xs) = PInfixApp (buildInPApp xs) expPProd (mkPIdent x)

expPProd :: QName
expPProd = (UnQual . Symbol) ":*:"

toEP :: Int -> Int -> VCInfo -> Pat
toEP _   1  vci = mkPRs $ conRecords vci
toEP cnt nc vci@(VCInfo _ _ i _ _ rs)
    | i == cnt + 1 && i == nc - 1 = ppPAppConUnQualIdent "R" $ mkPRs rs
    | i == cnt  = ppPAppConUnQualIdent "L" $ mkPRs rs
    | otherwise = PParen (PApp (unQualIdent "R") [(toEP (cnt + 1) nc vci)])


