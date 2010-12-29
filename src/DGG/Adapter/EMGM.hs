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
mkDTReps tci = [] {- map ($tci) [ mkRepFn,     mkRepInst
                          , mkFRepFn,    mkFRepInst
                          , mkFRep2Fn,   mkFRep2Inst
                          , mkBiFRep2Fn, mkBiFRep2Inst
                          , mkFRep3Fn,   mkFRep3Inst ] -}

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

mkGenG  = ClassA (unQualIdent "Generic")  [TyVar (Ident "g")]
mkGenG2 = ClassA (unQualIdent "Generic2") [TyVar (Ident "g")]
mkGenG3 = ClassA (unQualIdent "Generic3") [TyVar (Ident "g")]

mkRepName :: String -> String
mkRepName n = "dggRep_" ++ n

mkFRepName :: String -> String
mkFRepName n = "dggFRep_" ++ n

mkFRep2Name :: String -> String
mkFRep2Name n = "dggFRep2_" ++ n

mkBiFRep2Name :: String -> String
mkBiFRep2Name n = "dggBiFRep2_" ++ n

mkFRep3Name :: String -> String
mkFRep3Name n = "dggFRep3_" ++ n


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

mkFRepFn :: TCInfo -> Decl
mkFRepFn (TCInfo tn TyDataType vcis) = PatBind srcLoc (mkPIdent $ mkFRepName tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldInApp fnRSum mkSProd $ reverse vcis)))
    (BDecls [])

mkFRep2Fn :: TCInfo -> Decl
mkFRep2Fn (TCInfo tn TyDataType vcis) = PatBind srcLoc (mkPIdent $ mkFRep2Name tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldInApp fnRSum mkSProd $ reverse vcis)))
    (BDecls [])

mkBiFRep2Fn :: TCInfo -> Decl
mkBiFRep2Fn (TCInfo tn TyDataType vcis) = PatBind srcLoc (mkPIdent $ mkBiFRep2Name tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldInApp fnRSum mkSProd $ reverse vcis)))
    (BDecls [])

mkFRep3Fn :: TCInfo -> Decl
mkFRep3Fn (TCInfo tn TyDataType vcis) = PatBind srcLoc (mkPIdent $ mkFRep3Name tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldInApp fnRSum mkSProd $ reverse vcis)))
    (BDecls [])



    {-
-- TODO: Construct this next bit for every value constructor
    (InfixApp (App (App appCon (mkConDescr tn 0)
-- TODO: This next bit needs to be made dynamic
--        (Paren (App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2))))
    ) appRep) fnRProd (App appFrep appRep))
    -}
    
{-
--        (InfixApp (App (mkIdent "rtype") (mkIdent $ mkEPName tn)) fnApp
          
          (InfixApp (App (App (Var (UnQual (Ident "rcon"))) (Paren
(App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2
))))) (Var (UnQual (Ident "rep")))) (QVarOp (UnQual (Ident "rprod"))) (App (Var
(UnQual (Ident "frep"))) (Var (UnQual (Ident "rep"))))))
          
          (BDecls [])
-}

foldInApp :: QOp -> (a -> Exp) -> [a] -> Exp
foldInApp _  mk [x]    = mk x
foldInApp op mk (x:xs) = InfixApp (foldInApp op mk xs) op (mk x)

buildRepProd :: Int -> Exp
buildRepProd n = foldInApp fnRProd mkIdent $ replicate n "rep"

mkSProd :: VCInfo -> Exp
mkSProd (VCInfo n 0 _ _ _ _) = mkSProd' n 0 appRep
mkSProd (VCInfo n a _ _ _ _) = mkSProd' n a (buildRepProd a) 

mkSProd' :: String -> Int -> Exp -> Exp
mkSProd' n a r = App (App appCon (mkConDescr n (toInteger a))) r

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

mkFRepInst :: TCInfo -> Decl
mkFRepInst (TCInfo tn _ vcis) = InstDecl srcLoc
    [mkGenG{- TODO Add rest -}]
    (unQualIdent "FRep")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [TyVar (Ident "g"), TyCon (unQualIdent tn)]
    [InsDecl (PatBind srcLoc (PVar (Ident "frep")) Nothing
    (UnGuardedRhs (mkIdent $ mkFRepName tn)) (BDecls []))]

mkFRep2Inst :: TCInfo -> Decl
mkFRep2Inst (TCInfo tn _ vcis) = InstDecl srcLoc
    [mkGenG2{- TODO Add rest -}]
    (unQualIdent "FRep2")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [TyVar (Ident "g"), TyCon (unQualIdent tn)]
    [InsDecl (PatBind srcLoc (PVar (Ident "frep2")) Nothing
    (UnGuardedRhs (mkIdent $ mkFRep2Name tn)) (BDecls []))]

mkBiFRep2Inst :: TCInfo -> Decl
mkBiFRep2Inst (TCInfo tn _ vcis) = InstDecl srcLoc
    [mkGenG2{- TODO Add rest -}]
    (unQualIdent "BiFRep2")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [TyVar (Ident "g"), TyCon (unQualIdent tn)]
    [InsDecl (PatBind srcLoc (PVar (Ident "bifrep2")) Nothing
    (UnGuardedRhs (mkIdent $ mkBiFRep2Name tn)) (BDecls []))]

mkFRep3Inst :: TCInfo -> Decl
mkFRep3Inst (TCInfo tn _ vcis) = InstDecl srcLoc
    [mkGenG3{- TODO Add rest -}]
    (unQualIdent "FRep3")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [TyVar (Ident "g"), TyCon (unQualIdent tn)]
    [InsDecl (PatBind srcLoc (PVar (Ident "frep3")) Nothing
    (UnGuardedRhs (mkIdent $ mkFRep3Name tn)) (BDecls []))]

{-
PatBind srcLoc (PVar (Ident "rNonempty")) Nothing (

UnGuardedRhs (InfixApp (App
(Var (UnQual (Ident "rtype"))) (Var (UnQual (Ident "nonemptyEP"))))
(QVarOp (UnQual (Symbol "$")))
(InfixApp (App (App (Var (UnQual (Ident "rcon"))) (Paren
(App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2
))))) (Var (UnQual (Ident "rep")))) (QVarOp (UnQual (Ident "rprod"))) (App (Var
(UnQual (Ident "frep"))) (Var (UnQual (Ident "rep"))))))
) (BDecls [])

InstDecl srcLoc
[ ClassA (UnQual (Ident "Generic")) [TyVar (Ident "g")]
, ClassA (UnQual (Ident "Rep")) [TyVar (Ident "g"),TyVar (Ident "a")]
, ClassA (UnQual (Ident "FRep")) [TyVar (Ident "g"),TyVar (Ident "f")] ]
(UnQual (Ident "Rep"))
[ TyVar (Ident "g")
, TyParen (TyApp (TyApp (TyCon (UnQual (Ident "Nonempty"))) (TyVar (Ident "f"))) (TyVar (Ident "a"))) ]
[ InsDecl (PatBind srcLoc (PVar (Ident "rep")) Nothing (UnGuardedRhs (Var (
UnQual (Ident "rNonempty")))) (BDecls [])) ]


FunBind [Match srcLoc (Ident "frNonempty") [PVar (Ident "ra")] Nothing (UnGuardedRhs (InfixApp (App (Var (UnQual (Ident "rtype"))) (Var (UnQual (Ident "nonemptyEP")))) (QVarOp (UnQual (Symbol "$"))) (InfixApp (App (App (Var (UnQual (Ident "rcon"))) (Paren (App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2))))) (Var (UnQual (Ident "ra")))) (QVarOp (UnQual (Ident "rprod"))) (App (Var (UnQual (Ident "frep"))) (Var (UnQual (Ident "ra"))))))) (BDecls [])]

InstDecl srcLoc [ClassA (UnQual (Ident "Generic")) [TyVar (Ident "g")],ClassA (UnQual (Ident "FRep")) [TyVar (Ident "g"),TyVar (Ident "f")]] (UnQual (Ident "FRep")) [TyVar (Ident "g"),TyParen (TyApp (TyCon (UnQual (Ident "Nonempty"))) (TyVar (Ident "f")))] [InsDecl (PatBind srcLoc (PVar (Ident "frep")) Nothing (UnGuardedRhs (Var (UnQual (Ident "frNonempty")))) (BDecls []))]


FunBind [Match srcLoc (Ident "fr2Nonempty") [PVar (Ident "ra")] Nothing (UnGuardedRhs (InfixApp (App (App (Var (UnQual (Ident "rtype2"))) (Var (UnQual (Ident "nonemptyEP")))) (Var (UnQual (Ident "nonemptyEP")))) (QVarOp (UnQual (Symbol "$"))) (InfixApp (App (App (Var (UnQual (Ident "rcon2"))) (Paren (App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2))))) (Var (UnQual (Ident "ra")))) (QVarOp (UnQual (Ident "rprod2"))) (App (Var (UnQual (Ident "frep2"))) (Var (UnQual (Ident "ra"))))))) (BDecls [])]

InstDecl srcLoc [ClassA (UnQual (Ident "Generic2")) [TyVar (Ident "g")],ClassA (UnQual (Ident "FRep2")) [TyVar (Ident "g"),TyVar (Ident "f")]] (UnQual (Ident "FRep2")) [TyVar (Ident "g"),TyParen (TyApp (TyCon (UnQual (Ident "Nonempty"))) (TyVar (Ident "f")))] [InsDecl (PatBind srcLoc (PVar (Ident "frep2")) Nothing (UnGuardedRhs (Var (UnQual (Ident "fr2Nonempty")))) (BDecls []))]


mkTreeInst = InstDecl srcLoc
    [ ClassA (UnQual (Ident "Generic")) [TyVar (Ident "g")]
    , ClassA (UnQual (Ident "Rep")) [TyVar (Ident "g"),TyCon (UnQual (Ident "Int"))] ]
    (UnQual (Ident "Rep")) [TyVar (Ident "g"),TyCon (UnQual (Ident "Tree"))]
    [InsDecl (PatBind srcLoc (PVar (Ident "rep")) Nothing 
    (UnGuardedRhs

        (InfixApp (App (Var (UnQual (Ident "rtype"))) (Var (UnQual 
        (Ident "treeEP")))) (QVarOp (UnQual (Symbol "$"))) (InfixApp (InfixApp
        (App (App (Var (UnQual (Ident "rcon"))) (Paren (App (App (App (App (Con
        (UnQual (Ident "ConDescr"))) (Lit (String "Empty"))) (Lit (Int 0)))
        (Con (UnQual (Ident "False")))) (Con (UnQual (Ident "Prefix")))))) (Var
        (UnQual (Ident "runit")))) (QVarOp (UnQual (Ident "rsum"))) (App (App
        (Var (UnQual (Ident "rcon"))) (Paren (App (App (App (App (Con (UnQual
        (Ident "ConDescr"))) (Lit (String "Leaf"))) (Lit (Int 1))) (Con (UnQual
        (Ident "False")))) (Con (UnQual (Ident "Prefix")))))) (Var (UnQual
        (Ident "rint"))))) (QVarOp (UnQual (Ident "rsum"))) (App (App (Var
        (UnQual (Ident "rcon"))) (Paren (App (App (App (App (Con (UnQual
        (Ident "ConDescr"))) (Lit (String "Node"))) (Lit (Int 3))) (Con (UnQual
        (Ident "False")))) (Con (UnQual (Ident "Prefix")))))) (Paren (InfixApp
        (InfixApp (Var (UnQual (Ident "rep"))) (QVarOp (UnQual (Ident "rprod"))
        ) (Var (UnQual (Ident "rep")))) (QVarOp (UnQual (Ident "rprod"))) (Var
        (UnQual (Ident "rep"))))))))) 

        (BDecls []))]

mkCompInst = InstDecl srcLoc
    [ ClassA (UnQual (Ident "Generic")) [TyVar (Ident "g")]
    , ClassA (UnQual (Ident "Rep")) [TyVar (Ident "g"),TyVar (Ident "a")]
    , ClassA (UnQual (Ident "FRep")) [TyVar (Ident "g"),TyVar (Ident "f")]
    , ClassA (UnQual (Ident "FRep")) [TyVar (Ident "g"),TyVar (Ident "h")] ]
    (UnQual (Ident "Rep"))
    [TyVar (Ident "g"), TyParen (TyApp (TyApp (TyApp (TyCon (UnQual (Ident "Comp"))) (TyVar (Ident "f"))) (TyVar (Ident "h"))) (TyVar (Ident "a")))]
    [InsDecl (PatBind srcLoc (PVar (Ident "rep")) Nothing (UnGuardedRhs (Var (UnQual (Ident "rComp")))) (BDecls []))]

    -}
