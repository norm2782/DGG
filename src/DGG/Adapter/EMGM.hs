module DGG.Adapter.EMGM (
      makeEMGM
    , deriveEMGM
    , isSuppEMGM
    , importsEMGM
    ) where

import DGG.AdapterAbstract

importsEMGM :: [ImportDecl]
importsEMGM = [mkImport "Generics.EMGM"]

deriveEMGM :: Derivation
deriveEMGM = deriveLib "EMGM" makeEMGM

makeEMGM :: CodeGenerator
makeEMGM tc@(TCInfo _ TyDataType _ _) = (createDTEP tc) : (mkDTReps tc)
makeEMGM tc@(TCInfo _ TyNewType  _ _) = createNTEP   tc
makeEMGM tc@(TCInfo _ TySynonym  _ _) = createSynEP  tc
makeEMGM tc@(TCInfo _ TyGADT     _ _) = createGADTEP tc

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

isSuppEMGM' :: UnivSupp -> Bool
isSuppEMGM' Regular      = True
isSuppEMGM' HigherKinded = True
isSuppEMGM' Nested       = True
isSuppEMGM' NestedHigherKinded = True
isSuppEMGM' OtherH98     = True
isSuppEMGM' SubUniv      = True
-- TODO: Verify truth values
isSuppEMGM' HigherRankCon      = False
isSuppEMGM' ExistentialTypes   = False
isSuppEMGM' SuppGADTs = False
isSuppEMGM' MutRec    = False

mkDTReps :: TCInfo -> [Decl]
mkDTReps tci = [] {- map ($tci) [ mkRepFn,     mkRepInst
                          , mkFRepFn,    mkFRepInst
                          , mkFRep2Fn,   mkFRep2Inst
                          , mkBiFRep2Fn, mkBiFRep2Inst
                          , mkFRep3Fn,   mkFRep3Inst ] -}

mkEPName :: Name -> String
mkEPName n = "dggEP_" ++ (fromName n)

createDTEP :: TCInfo -> Decl
createDTEP (TCInfo tn TyDataType _ vcis) =
  PatBind srcLoc (mkPIdent $ mkEPName tn) Nothing
  (UnGuardedRhs (App (App (Con $ mkUId "EP") (mkIdent fromFunName))
  (mkIdent toFunName))) (BDecls [ FunBind $ map (bdeclFrom ln) vcis
                                , FunBind $ map (bdeclTo ln) vcis ])
  where ln = length vcis

createNTEP   (TCInfo tn TyNewType _ vcis) = undefined
createSynEP  (TCInfo tn TySynonym _ vcis) = undefined
createGADTEP (TCInfo tn TyGADT    _ vcis) = undefined

fromFunName, toFunName, unitType :: String
fromFunName = "from'"
toFunName   = "to'"
unitType    = "Unit"

--    --
--    --
-- EP --
--    --
--    --
--
-- From
bdeclFrom :: Int -> DCInfo -> Match
bdeclFrom cnt dci = mkMatch fromFunName [pApp n (map mkPIdent (genNames a))] 
    (fromEP 0 cnt dci)
    where n = dcName dci
          a = dcArity dci

owFrom :: Int -> Int -> DCInfo -> Exp
owFrom cnt nc dci = App (mkStrCon "R") (fromEP (cnt + 1) nc dci)

fromEP :: Int -> Int -> DCInfo -> Exp
fromEP = ep mkFromRs mkExpSum owFrom

mkFromRs :: Int -> Exp
mkFromRs 0 = mkStrCon unitType
mkFromRs n = foldlInApp (QConOp . unQualSym $ ":*:") mkIdent $ genNames n

mkExpSum :: String -> Int -> Exp
mkExpSum s n = (App . mkStrCon) s $ mkFromRs n


-- To
bdeclTo :: Int -> DCInfo -> Match
bdeclTo cnt dci = mkMatch toFunName [toEP 0 cnt dci] (mkToRhs dci)

owTo :: Int -> Int -> DCInfo -> Pat
owTo cnt nc dci = PApp (mkUId "R") [(toEP (cnt + 1) nc dci)]

toEP :: Int -> Int -> DCInfo -> Pat
toEP = ep mkToRs mkPatSum owTo

mkToRs :: Int -> Pat
mkToRs 0 = pApp (name unitType) []
mkToRs n = foldlPInApp (unQualSym ":*:") mkPIdent (genNames n)

mkToRhs :: DCInfo -> Exp
mkToRhs dci | a == 0    = mkNCon n
            | otherwise = appFun (mkNCon n) (map mkIdent $ genNames a)
            where n = dcName dci
                  a = dcArity dci

mkPatSum :: String -> Int -> Pat
mkPatSum s n = pApp (name s) [mkToRs n]


-- Rest
ep :: (Int -> a) -> (String -> Int -> a) -> (Int -> Int -> DCInfo -> a)
   -> Int -> Int -> DCInfo -> a
ep mkr mks ow _   1  dci = mkr $ dcArity dci
ep mkr mks ow cnt nc dci | i == cnt + 1 && i == nc - 1 = mks "R" a
                         | i == cnt                    = mks "L" a
                         | otherwise                   = ow cnt nc dci
                         where a = dcArity dci
                               i = dcIndex dci

mkGenG  = ClassA (mkUId "Generic")  [mkTyVar "g"]
mkGenG2 = ClassA (mkUId "Generic2") [mkTyVar "g"]
mkGenG3 = ClassA (mkUId "Generic3") [mkTyVar "g"]

mkRepName :: Name -> String
mkRepName n = "dggRep_" ++ fromName n

mkFRepName :: Name -> String
mkFRepName n = "dggFRep_" ++ fromName n

mkFRep2Name :: Name -> String
mkFRep2Name n = "dggFRep2_" ++ fromName n

mkBiFRep2Name :: Name -> String
mkBiFRep2Name n = "dggBiFRep2_" ++ fromName n

mkFRep3Name :: Name -> String
mkFRep3Name n = "dggFRep3_" ++ fromName n

fnApp   = (QVarOp . unQualSym) "$"
fnRProd = appInfix "rprod"
fnRSum  = appInfix "rsum"
appRep  = mkIdent "rep"
appUnit = mkIdent "runit"
appInt  = mkIdent "rint"
appCon  = mkIdent "rcon"
appFrep = mkIdent "frep"

-- TODO: A lot of the code below is copy/pasted to make things compile.
-- Obviously the code is incomplete and commonalities need to be extracted and
-- the code refactored.
mkRepFn :: TCInfo -> Decl
mkRepFn (TCInfo tn TyDataType tcv vcis) = PatBind srcLoc (mkPIdent $ mkRepName tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldlInApp fnRSum mkSProd $ reverse vcis)))
    bdecls

mkFRepFn :: TCInfo -> Decl
mkFRepFn (TCInfo tn TyDataType tcv vcis) = PatBind srcLoc (mkPIdent $ mkFRepName tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldlInApp fnRSum mkSProd $ reverse vcis)))
    bdecls

mkFRep2Fn :: TCInfo -> Decl
mkFRep2Fn (TCInfo tn TyDataType tcv vcis) = PatBind srcLoc (mkPIdent $ mkFRep2Name tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldlInApp fnRSum mkSProd $ reverse vcis)))
    bdecls

mkBiFRep2Fn :: TCInfo -> Decl
mkBiFRep2Fn (TCInfo tn TyDataType tcv vcis) = PatBind srcLoc (mkPIdent $ mkBiFRep2Name tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldlInApp fnRSum mkSProd $ reverse vcis)))
    bdecls

mkFRep3Fn :: TCInfo -> Decl
mkFRep3Fn (TCInfo tn TyDataType tcv vcis) = PatBind srcLoc (mkPIdent $ mkFRep3Name tn)
    Nothing (UnGuardedRhs (InfixApp (App (mkIdent "rtype")
    (mkIdent $ mkEPName tn)) fnApp (foldlInApp fnRSum mkSProd $ reverse vcis)))
    bdecls



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
          
          bdecls
-}

buildRepProd :: Int -> Exp
buildRepProd n = foldlInApp fnRProd mkIdent $ replicate n "rep"

mkSProd :: DCInfo -> Exp
mkSProd dci | a == 0    = mkSProd' n 0 appRep
            | otherwise = mkSProd' n a (buildRepProd a) 
            where a = dcArity dci
                  n = dcName dci

mkSProd' :: Name -> Int -> Exp -> Exp
mkSProd' n a r = foldApp id $ reverse [appCon, mkConDescr n (toInteger a), r]

-- TODO: Make constructor information dynamic
mkConDescr :: Name -> Integer -> Exp
mkConDescr n a = Paren (App (App (App (App (Con
        (mkUId "ConDescr")) (mkStrLit n)) (Lit (Int a)))
        (mkStrCon "False")) (mkStrCon "Prefix"))

mkRepInst :: TCInfo -> Decl
mkRepInst (TCInfo tn _ tcv vcis) = InstDecl srcLoc
    [mkGenG{- TODO Add rest -}]
    (mkUId "Rep")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [mkTyVar "g", mkTyCon tn]
    [InsDecl (PatBind srcLoc (mkPIdent "rep") Nothing
    (UnGuardedRhs (mkIdent $ mkRepName tn)) bdecls)]

mkFRepInst :: TCInfo -> Decl
mkFRepInst (TCInfo tn _ tcv vcis) = InstDecl srcLoc
    [mkGenG{- TODO Add rest -}]
    (mkUId "FRep")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [mkTyVar "g", mkTyCon tn]
    [InsDecl (PatBind srcLoc (mkPIdent "frep") Nothing
    (UnGuardedRhs (mkIdent $ mkFRepName tn)) bdecls)]

mkFRep2Inst :: TCInfo -> Decl
mkFRep2Inst (TCInfo tn _ tcv vcis) = InstDecl srcLoc
    [mkGenG2{- TODO Add rest -}]
    (mkUId "FRep2")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [mkTyVar "g", mkTyCon tn]
    [InsDecl (PatBind srcLoc (mkPIdent "frep2") Nothing
    (UnGuardedRhs (mkIdent $ mkFRep2Name tn)) bdecls)]

mkBiFRep2Inst :: TCInfo -> Decl
mkBiFRep2Inst (TCInfo tn _ tcv vcis) = InstDecl srcLoc
    [mkGenG2{- TODO Add rest -}]
    (mkUId "BiFRep2")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [mkTyVar "g", mkTyCon tn]
    [InsDecl (PatBind srcLoc (mkPIdent "bifrep2") Nothing
    (UnGuardedRhs (mkIdent $ mkBiFRep2Name tn)) bdecls)]

mkFRep3Inst :: TCInfo -> Decl
mkFRep3Inst (TCInfo tn _ tcv vcis) = InstDecl srcLoc
    [mkGenG3{- TODO Add rest -}]
    (mkUId "FRep3")
-- TODO: The TyCon currently only supports kind *. Expand support to 
-- include arbitrarily kinded datatypes.
    [mkTyVar "g", mkTyCon tn]
    [InsDecl (PatBind srcLoc (mkPIdent "frep3") Nothing
    (UnGuardedRhs (mkIdent $ mkFRep3Name tn)) bdecls)]

{-
PatBind srcLoc (PVar (Ident "rNonempty")) Nothing (

UnGuardedRhs (InfixApp (App
(Var (UnQual (Ident "rtype"))) (Var (UnQual (Ident "nonemptyEP"))))
(QVarOp (UnQual (Symbol "$")))
(InfixApp (App (App (Var (UnQual (Ident "rcon"))) (Paren
(App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2
))))) (Var (UnQual (Ident "rep")))) (QVarOp (UnQual (Ident "rprod"))) (App (Var
(UnQual (Ident "frep"))) (Var (UnQual (Ident "rep"))))))
) bdecls

InstDecl srcLoc
[ ClassA (UnQual (Ident "Generic")) [mkTyVar "g"]
, ClassA (UnQual (Ident "Rep")) [mkTyVar "g",TyVar (Ident "a")]
, ClassA (UnQual (Ident "FRep")) [mkTyVar "g",TyVar (Ident "f")] ]
(UnQual (Ident "Rep"))
[ mkTyVar "g"
, TyParen (TyApp (TyApp (TyCon (UnQual (Ident "Nonempty"))) (TyVar (Ident "f"))) (TyVar (Ident "a"))) ]
[ InsDecl (PatBind srcLoc (PVar (Ident "rep")) Nothing (UnGuardedRhs (Var (
UnQual (Ident "rNonempty")))) bdecls) ]


FunBind [Match srcLoc (Ident "frNonempty") [PVar (Ident "ra")] Nothing (UnGuardedRhs (InfixApp (App (Var (UnQual (Ident "rtype"))) (Var (UnQual (Ident "nonemptyEP")))) (QVarOp (UnQual (Symbol "$"))) (InfixApp (App (App (Var (UnQual (Ident "rcon"))) (Paren (App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2))))) (Var (UnQual (Ident "ra")))) (QVarOp (UnQual (Ident "rprod"))) (App (Var (UnQual (Ident "frep"))) (Var (UnQual (Ident "ra"))))))) bdecls]

InstDecl srcLoc [ClassA (UnQual (Ident "Generic")) [mkTyVar "g"],ClassA (UnQual (Ident "FRep")) [mkTyVar "g",TyVar (Ident "f")]] (UnQual (Ident "FRep")) [mkTyVar "g",TyParen (TyApp (TyCon (UnQual (Ident "Nonempty"))) (TyVar (Ident "f")))] [InsDecl (PatBind srcLoc (PVar (Ident "frep")) Nothing (UnGuardedRhs (Var (UnQual (Ident "frNonempty")))) bdecls)]


FunBind [Match srcLoc (Ident "fr2Nonempty") [PVar (Ident "ra")] Nothing (UnGuardedRhs (InfixApp (App (App (Var (UnQual (Ident "rtype2"))) (Var (UnQual (Ident "nonemptyEP")))) (Var (UnQual (Ident "nonemptyEP")))) (QVarOp (UnQual (Symbol "$"))) (InfixApp (App (App (Var (UnQual (Ident "rcon2"))) (Paren (App (App (Var (UnQual (Ident "mkCon"))) (Lit (String "Nonempty"))) (Lit (Int 2))))) (Var (UnQual (Ident "ra")))) (QVarOp (UnQual (Ident "rprod2"))) (App (Var (UnQual (Ident "frep2"))) (Var (UnQual (Ident "ra"))))))) bdecls]

InstDecl srcLoc [ClassA (UnQual (Ident "Generic2")) [mkTyVar "g"],ClassA (UnQual (Ident "FRep2")) [mkTyVar "g",TyVar (Ident "f")]] (UnQual (Ident "FRep2")) [mkTyVar "g",TyParen (TyApp (TyCon (UnQual (Ident "Nonempty"))) (TyVar (Ident "f")))] [InsDecl (PatBind srcLoc (PVar (Ident "frep2")) Nothing (UnGuardedRhs (Var (UnQual (Ident "fr2Nonempty")))) bdecls)]


mkTreeInst = InstDecl srcLoc
    [ ClassA (UnQual (Ident "Generic")) [mkTyVar "g"]
    , ClassA (UnQual (Ident "Rep")) [mkTyVar "g",TyCon (UnQual (Ident "Int"))] ]
    (UnQual (Ident "Rep")) [mkTyVar "g",TyCon (UnQual (Ident "Tree"))]
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

        bdecls)]

mkCompInst = InstDecl srcLoc
    [ ClassA (UnQual (Ident "Generic")) [mkTyVar "g"]
    , ClassA (UnQual (Ident "Rep")) [mkTyVar "g",TyVar (Ident "a")]
    , ClassA (UnQual (Ident "FRep")) [mkTyVar "g",TyVar (Ident "f")]
    , ClassA (UnQual (Ident "FRep")) [mkTyVar "g",TyVar (Ident "h")] ]
    (UnQual (Ident "Rep"))
    [mkTyVar "g", TyParen (TyApp (TyApp (TyApp (TyCon (UnQual (Ident "Comp"))) (TyVar (Ident "f"))) (TyVar (Ident "h"))) (TyVar (Ident "a")))]
    [InsDecl (PatBind srcLoc (PVar (Ident "rep")) Nothing (UnGuardedRhs (Var (UnQual (Ident "rComp")))) bdecls)]

    -}
