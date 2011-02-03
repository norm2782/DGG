module DGG.Adapter.SYB (
      deriveSYB
    , importsSYB
    , isSuppSYB
    , makeSYB
    ) where

import DGG.AdapterAbstract

importsSYB :: [ImportDecl]
importsSYB = [mkImport "Data.Data", mkImport "Data.Generics"]

deriveSYB :: Derivation
deriveSYB = deriveLib "SYB" makeSYB

makeSYB :: CodeGenerator
makeSYB tc@(TCInfo n TyDataType tvs vcs) = (mkTypeableNs tc) ++
                                           [mkData tc, mkDT tc] ++
                                           map (mkConstr n) vcs

-- Returns True when the Decl is of the right type and False otherwise. Several
-- types return False at the moment, because they are not supported yet by this
-- library. Support for these types is planned for future increments.
-- TODO: Perhaps this check should be performed on the DGG custom datatypes
-- instead? They can tell if a datatype is mutually recursive, etc.
isSuppSYB :: Decl -> Bool
isSuppSYB (TypeDecl _ _ _ _)          = False
isSuppSYB (TypeFamDecl _ _ _ _)       = False
isSuppSYB (DataDecl _ _ _ _ _ _ _)    = True
isSuppSYB (GDataDecl _ _ _ _ _ _ _ _) = False
isSuppSYB (DataFamDecl _ _ _ _ _)     = False
isSuppSYB _                           = False

isSuppSYB' :: UnivSupp -> Bool
isSuppSYB' Regular      = True
isSuppSYB' HigherKinded = True
isSuppSYB' Nested       = True
isSuppSYB' NestedHigherKinded = False
isSuppSYB' OtherH98     = True
isSuppSYB' SubUniv      = False
-- TODO: Verify truth values
isSuppSYB' HigherRankCon = False
isSuppSYB' ExistentialTypes = False
isSuppSYB' SuppGADTs        = False
isSuppSYB' MutRec    = False

-- TODO: No gunfold when quantification is used?

-- TODO: For these next three we need more information about the kinds. The
-- current approach for determining the kind of datatypes is plain wrong: it
-- just counts the number of tycon arguments. Instead, it should take the kind
-- of those arguments into account.
mkTypeableNs :: TCInfo -> [Decl]
mkTypeableNs tci | n == 0           = [mkTypeable tci]
                 | n >= 1 && n <= 7 = [mkTypeableN tci]
                 | otherwise        = []
                 where n = length $ tcVars tci

-- TODO: Refactor the next two: lots of shared stuff here!
mkTypeable :: TCInfo -> Decl
mkTypeable (TCInfo n _ tvs _) = InstDecl srcLoc [] (mkUId "Typeable")
    (mkClassInst n tvs)
    [mkInFun [Match srcLoc (Ident "typeOf") [PWildCard] Nothing (
    UnGuardedRhs (App (App (mkIdent "mkTyConApp") (App (mkIdent "mkTyCon")
    (mkStrLit n))) (List []))) bdecls]]

mkTypeableN :: TCInfo -> Decl
mkTypeableN (TCInfo n _ tvs _) = InstDecl srcLoc [] (mkUId $ "Typeable" ++ show lt)
    (mkClassInstN (lt - 1) n)
    [mkInFun [Match srcLoc (Ident $ "typeOf" ++ show lt) [PWildCard] Nothing (
    UnGuardedRhs (App (App (mkIdent "mkTyConApp") (App (mkIdent "mkTyCon")
    (mkStrLit n))) (List []))) bdecls]]
    where lt = length tvs

mkClassInstN :: Int -> Name -> [Type]
mkClassInstN i n = [foldTyApp id $ reverse
                   $ (mkTyCon n) : (map mkTyVar $ genNames i)]

mkClassReq :: String -> Asst
mkClassReq n = ClassA (mkUId "Data") [mkTyVar n]

mkClassInst :: Name -> [TCVar] -> [Type]
mkClassInst n tvs = [foldTyApp id $ reverse
                  $ (mkTyCon n) : (map mkTyVar $ genNames $ length tvs)]

mkDTName :: Name -> String
mkDTName n = "dggDT_" ++ fromName n

mkData :: TCInfo -> Decl
mkData (TCInfo n _ tvs vcs) = InstDecl srcLoc
    (map mkClassReq $ genNames $ length tvs) (mkUId "Data") (mkClassInst n tvs)
    ([mkInFun (map mkToConstr vcs),
    mkInFun [Match srcLoc (Ident "dataTypeOf") [PWildCard] Nothing
                     (UnGuardedRhs (mkIdent $ mkDTName n)) bdecls],
    mkInFun (map mkGfoldl vcs),
    mkInFun $ mkGunfold vcs] ++ ((mkDataCast . length) tvs)
    )

-- TODO: Verify that kind information is used correctly this way
mkDataCast :: Int -> [InstDecl]
mkDataCast 1 = mkDCID "dataCast1" "gcast1"
mkDataCast 2 = mkDCID "dataCast2" "gcast2"
mkDataCast _ = []

mkDCID :: String -> String -> [InstDecl]
mkDCID lhs rhs = [mkInFun [mkMatch lhs [mkPIdent "f"] (App (mkIdent rhs) (mkIdent "f"))]]

mkInFun :: [Match] -> InstDecl
mkInFun = InsDecl . FunBind

mkGunfold :: [DCInfo] -> [Match]
mkGunfold vcs = [mkMatch "gunfold" [mkPIdent "k", mkPIdent "z", mkPIdent "c"]
    (Case (App (mkIdent "constrIndex") (mkIdent "c"))
    ((map mkGunfoldAlt $ zip [1..] vcs) ++ [Alt srcLoc PWildCard (UnGuardedAlt
     (App (mkIdent "error") (Lit (String "gunfold: no match for ctor index"))))
      bdecls])
    )]

mkGunfoldAlt :: (Int, DCInfo) -> Alt
mkGunfoldAlt (i, dci) = Alt srcLoc (PLit (Int (toInteger i)))
    (UnGuardedAlt (mkGunfoldKs n a)) bdecls
    where n = dcName dci
          a = dcArity dci

mkGunfoldKs :: Name -> Int -> Exp
mkGunfoldKs n 0 = App (mkIdent "z") (mkNCon n)
mkGunfoldKs n a = Paren (App (mkIdent "k") (mkGunfoldKs n $ a - 1))

mkToConstr :: DCInfo -> Match
mkToConstr dci = mkMatch "toConstr" [PApp (UnQual n) (replicate a PWildCard)]
    (mkIdent $ mkConstrName n)
    where n = dcName dci
          a = dcArity dci

mkGfoldl :: DCInfo -> Match
mkGfoldl dci = mkMatch "gfoldl" [mkPIdent "k", mkPIdent "z", PApp (UnQual n)
    (map mkPIdent $ genNames a)] (foldInApp (appInfix "k") id $ reverse $ App
    (mkIdent "z") (mkNCon n) : (map mkIdent $ genNames a))
    where n = dcName dci
          a = dcArity dci

mkDT (TCInfo n _ _ vcs) = PatBind srcLoc (mkPIdent $ mkDTName n) Nothing
    (UnGuardedRhs (App (App (mkIdent "mkDataType") (Lit (String (fromName n)))) (List
    (map (mkIdent . mkConstrName . dcName) vcs)))) bdecls

mkConstrName :: Name -> String
mkConstrName n = "dggConstr_" ++ fromName n

mkConstr :: Name -> DCInfo -> Decl
mkConstr tcn dci = PatBind srcLoc (mkPIdent $ mkConstrName n) Nothing
    (UnGuardedRhs (App (App (App (App (mkIdent "mkConstr")
    (mkIdent $ mkDTName tcn)) (mkStrLit n)) (List [])
--    (List [Lit (String "lTree"),Lit (String "bVal"),Lit (String "rTree")]) TODO: Record names
    ) (Con (UnQual(Ident "Prefix"))))) bdecls -- TODO: Fixity
    where n = dcName dci

{-
DataDecl srcLoc DataType [] (Ident "MyTree") [UnkindedVar (Ident "a")] [
QualConDecl srcLoc [] [] (ConDecl (Ident "MyLeaf") []),QualConDecl srcLoc [] []
(RecDecl (Ident "MyBinNode") [([Ident "lTree"],UnBangedTy (TyParen (TyApp (
TyCon (UnQual (Ident "MyTree"))) (TyVar (Ident "a"))))),([Ident "bVal"],
UnBangedTy (TyVar (Ident "a"))),([Ident "rTree"],UnBangedTy (TyParen (TyApp (
TyCon (UnQual (Ident "MyTree"))) (TyVar (Ident "a")))))]),QualConDecl srcLoc []
[] (ConDecl (Ident "MyRTree") [UnBangedTy (TyVar (Ident "a")),UnBangedTy (
TyList (TyApp (TyCon (UnQual (Ident "MyTree"))) (TyVar (Ident "a"))))])] []

InstDecl srcLoc [] (mkUId "Typeable")
[TyApp (mkTyCon "MyTree") (mkTyVar "a")]
[InsDecl (FunBind [Match srcLoc (Ident "typeOf") [PWildCard] Nothing
(UnGuardedRhs (App (App (mkIdent "mkTyConApp") (App (mkIdent "mkTyCon")
(Lit (String "MyTree")))) (List []))) bdecls])]

InstDecl srcLoc [ClassA (UnQual (Ident "Data")) [TyVar (Ident "a")]] (UnQual (
Ident "Data")) [TyParen (TyApp (TyCon (UnQual (Ident "MyTree"))) (TyVar (Ident
"a")))] [InsDecl (FunBind [Match srcLoc (Ident "toConstr") [PApp (UnQual (Ident
"MyLeaf")) []] Nothing (UnGuardedRhs (Var (UnQual (Ident "myLeafConstr")))) (
BDecls []),Match srcLoc (Ident "toConstr") [PParen (PApp (UnQual (Ident
"MyBinNode")) [PWildCard,PWildCard,PWildCard])] Nothing (UnGuardedRhs (Var (
UnQual (Ident "myBinNConstr")))) bdecls,Match srcLoc (Ident "toConstr")
[PParen (PApp (UnQual (Ident "MyRTree")) [PWildCard,PWildCard])] Nothing (
UnGuardedRhs (Var (UnQual (Ident "myRTreeConstr")))) bdecls]),InsDecl (
FunBind [Match srcLoc (Ident "dataTypeOf") [PWildCard] Nothing (UnGuardedRhs (
Var (UnQual (Ident "myTreeDT")))) bdecls]),InsDecl (FunBind [Match srcLoc
(Ident "gfoldl") [PVar (Ident "k"),PVar (Ident "z"),PApp (UnQual (Ident
"MyLeaf")) []] Nothing (UnGuardedRhs (App (Var (UnQual (Ident "z"))) (Con (
UnQual (Ident "MyLeaf"))))) bdecls,Match srcLoc (Ident "gfoldl") [PVar (
Ident "k"),PVar (Ident "z"),PParen (PApp (UnQual (Ident "MyBinNode")) [PVar (
Ident "t1"),PVar (Ident "a"),PVar (Ident "t2")])] Nothing (UnGuardedRhs (
InfixApp (InfixApp (InfixApp (App (Var (UnQual (Ident "z"))) (Con (UnQual (
Ident "MyBinNode")))) (appInfix "k") (Var (UnQual (Ident "t1")))
) (appInfix "k") (Var (UnQual (Ident "a")))) (QVarOp (UnQual (
Ident "k"))) (Var (UnQual (Ident "t2"))))) bdecls,Match srcLoc (Ident
"gfoldl") [PVar (Ident "k"),PVar (Ident "z"),PParen (PApp (UnQual (Ident
"MyRTree")) [PVar (Ident "a"),PVar (Ident "t")])] Nothing (UnGuardedRhs (
InfixApp (InfixApp (App (Var (UnQual (Ident "z"))) (Con (UnQual (Ident
"MyRTree")))) (appInfix "k") (Var (UnQual (Ident "a")))) (QVarOp
(UnQual (Ident "k"))) (Var (UnQual (Ident "t"))))) bdecls]),InsDecl (
FunBind [Match srcLoc (Ident "gunfold") [PVar (Ident "k"),PVar (Ident "z"),PVar
(Ident "c")] Nothing (UnGuardedRhs (Case (App (Var (UnQual (Ident "constrIndex"
))) (Var (UnQual (Ident "c")))) [Alt srcLoc (PLit (Int 1)) (UnGuardedAlt (App (
Var (UnQual (Ident "z"))) (Con (UnQual (Ident "MyLeaf"))))) bdecls,Alt
srcLoc (PLit (Int 2)) (UnGuardedAlt (App (Var (UnQual (Ident "k"))) (Paren (App
(Var (UnQual (Ident "k"))) (Paren (App (Var (UnQual (Ident "k"))) (Paren (App (
Var (UnQual (Ident "z"))) (Con (UnQual (Ident "MyBinNode"))))))))))) (BDecls []
),Alt srcLoc (PLit (Int 3)) (UnGuardedAlt (Paren (App (Var (UnQual (Ident "k"))
) (Paren (App (Var (UnQual (Ident "k"))) (Paren (App (Var (UnQual (Ident "z")))
(Con (UnQual (Ident "MyRTree")))))))))) bdecls,Alt srcLoc PWildCard (
UnGuardedAlt (App (Var (UnQual (Ident "error"))) (Lit (String "")))) (BDecls []
)])) bdecls])]

PatBind srcLoc (PVar (Ident "myTreeDT")) Nothing (UnGuardedRhs (App (App (Var (
UnQual (Ident "mkDataType"))) (Lit (String "MyTree"))) (List [Var (UnQual (
Ident "myLeafConstr")),Var (UnQual (Ident "myBinNConstr")),Var (UnQual (Ident
"myRTreeConstr"))]))) bdecls

PatBind srcLoc (PVar (Ident "myLeafConstr")) Nothing (UnGuardedRhs (App (App (
App (App (Var (UnQual (Ident "mkConstr"))) (Var (UnQual (Ident "myTreeDT")))) (
Lit (String "MyLeaf"))) (List [])) (Con (UnQual (Ident "Prefix")))))
bdecls

PatBind srcLoc (PVar (Ident "myBinNConstr")) Nothing (UnGuardedRhs (App (App (
App (App (Var (UnQual (Ident "mkConstr"))) (Var (UnQual (Ident "myTreeDT")))) (
Lit (String "MyBinNode"))) (List [Lit (String "lTree"),Lit (String "bVal"),Lit
(String "rTree")])) (Con (UnQual (Ident "Prefix"))))) bdecls

PatBind srcLoc (PVar (Ident "myRTreeConstr")) Nothing (UnGuardedRhs (App (App (
App (App (Var (UnQual (Ident "mkConstr"))) (Var (UnQual (Ident "myTreeDT")))) (
Lit (String "MyRTree"))) (List [])) (Con (UnQual (Ident "Prefix")))))
bdecls
-}
