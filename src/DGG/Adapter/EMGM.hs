module DGG.Adapter.EMGM (
      makeEMGM
    , deriveEMGM
    , isSuppEMGM
    , importsEMGM
    ) where

import Data.Derive.Internal.Derivation
import DGG.Adapter
import DGG.Data
import DGG.Parser
import Language.Haskell hiding (genNames)

importsEMGM :: [ImportDecl]
importsEMGM = [mkImport "Generics.EMGM"]

deriveEMGM :: Derivation
deriveEMGM = derivationCustom "DGG.Adapter.EMGM.Derivation" mkFullDecl

mkFullDecl :: FullDataDecl -> Either String [Decl]
mkFullDecl (_, decl) = Right $ (makeEMGM . mkTCI) decl

makeEMGM :: LibParser
makeEMGM tc@(TCInfo _ TyDataType _) = (createDTEP tc) : (mkDTReps tc)
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

mkDTReps :: TCInfo -> [Decl]
mkDTReps tci = map ($tci) [mkRepFn, mkRepInst]

mkEPName n = "dggEP_" ++ n

createDTEP :: TCInfo -> Decl
createDTEP (TCInfo tn TyDataType vcis) =
  PatBind srcLoc (mkPIdent $ mkEPName tn) Nothing rhs (bdecls vcis)

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
bdecls vcis = BDecls [ FunBind $ map (bdeclFrom ln) vcis
                     , FunBind $ map (bdeclTo ln) vcis ]
    where ln = length vcis

bdeclFrom :: Int -> VCInfo -> Match
bdeclFrom cnt vci@(VCInfo n a _ _ _ _) = mkMatch fromFunName [pApp (name n)
                                                 (map mkPIdent (genNames a))]
                                                 (fromEP 0 cnt vci)

bdeclTo :: Int -> VCInfo -> Match
bdeclTo cnt vci = mkMatch toFunName [toEP 0 cnt vci] (mkToRhs vci)

mkMatch :: String -> [Pat] -> Exp -> Match
mkMatch n ps rhs = Match srcLoc (Ident n) ps Nothing
                         (UnGuardedRhs rhs) (BDecls [])

mkToRhs :: VCInfo -> Exp
mkToRhs (VCInfo n 0 _ _ _ _) = mkCon n
mkToRhs (VCInfo n a _ _ _ _) = appFun (mkCon n) (map mkIdent $ genNames a)

buildProd :: Int -> Exp
buildProd n = foldInApp expProd mkIdent $ reverse (genNames n)

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
ppPAppConUnQualIdent s e = pApp (name s) [e]

mkPRs :: Int -> Pat
mkPRs 0  = pApp (name unitType) []
mkPRs i = buildPProd i

buildPProd :: Int -> Pat
buildPProd rs = buildInPApp $ reverse (genNames rs)

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

mkGenG = ClassA (unQualIdent "Generic") [TyVar (Ident "g")]

mkRepName :: String -> String
mkRepName n = "dggRep_" ++ n

fnApp   = (QVarOp . unQualSym) "$"
fnRProd = appInfix "rprod"
fnRSum  = appInfix "rsum"
appRep  = mkIdent "rep"
appUnit = mkIdent "runit"
appInt  = mkIdent "rint"
appCon  = mkIdent "rcon"
appFrep = mkIdent "frep"

mkRepFn :: TCInfo -> Decl
mkRepFn (TCInfo tn TyDataType vcis) = PatBind srcLoc (mkPIdent $ mkRepName tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldInApp fnRSum mkSProd $ reverse vcis)))
    (BDecls [])

foldInApp :: QOp -> (a -> Exp) -> [a] -> Exp
foldInApp _  mk [x]    = mk x
foldInApp op mk (x:xs) = InfixApp (foldInApp op mk xs) op (mk x)

buildRepProd :: Int -> Exp
buildRepProd n = foldInApp fnRProd mkIdent $ replicate n "rep"

mkSProd :: VCInfo -> Exp
mkSProd (VCInfo n 0 _ _ _ _) = App (App appCon (mkConDescr n 0)) appRep
mkSProd (VCInfo n a _ _ _ _) = App (App appCon (mkConDescr n (toInteger a)))
                                   (buildRepProd a)

-- TODO: Make constructor information dynamic
mkConDescr :: String -> Integer -> Exp
mkConDescr n a = Paren (App (App (App (App (Con
        (unQualIdent "ConDescr")) (Lit (String n))) (Lit (Int a)))
        (mkCon "False")) (mkCon "Prefix"))

mkRepInst :: TCInfo -> Decl
mkRepInst (TCInfo tn _ vcis) = InstDecl srcLoc
    [mkGenG{- TODO Add rest -}]
    (unQualIdent "Rep")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [TyVar (Ident "g"), TyCon (unQualIdent tn)]
    [InsDecl (PatBind srcLoc (PVar (Ident "rep")) Nothing
    (UnGuardedRhs (mkIdent $ mkRepName tn)) (BDecls []))]

