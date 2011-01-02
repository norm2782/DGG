module DGG.Adapter.SYB (
      deriveSYB
    , importsSYB
    , isSuppSYB
    , makeSYB
    ) where

import Data.Derive.Internal.Derivation
import DGG.Adapter
import DGG.Data
import DGG.Parser
import Language.Haskell hiding (genNames)

importsSYB :: [ImportDecl]
importsSYB = [mkImport "Data.Data", mkImport "Data.Generics"]

deriveSYB :: Derivation
deriveSYB = derivationCustom "DGG.Adapter.SYB.Derivation" mkFullDecl

mkFullDecl :: FullDataDecl -> Either String [Decl]
mkFullDecl (_, decl) = Right $ (makeSYB . mkTCI) decl

makeSYB :: LibParser
makeSYB tc@(TCInfo n TyDataType _ vcs) = [mkTypeable tc, mkData tc, mkDT tc] ++
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

-- TODO: No gunfold when quantification is used?
-- TODO: Defined dataCast1  = gcast1 when perfect datatype? or when nested?
mkTypeable :: TCInfo -> Decl
mkTypeable (TCInfo n _ tvs vcs) = InstDecl srcLoc [] (mkUId "Typeable")
    (mkClassInst n tvs)
    [mkInsDecl [Match srcLoc (Ident "typeOf") [PWildCard] Nothing (
    UnGuardedRhs (App (App (mkIdent "mkTyConApp") (App (mkIdent "mkTyCon") (Lit
    (String n)))) (List []))) bdecls]]

mkClassReq :: String -> Asst
mkClassReq n = ClassA (mkUId "Data") [mkTyVar n]

mkClassInst :: String -> [TyVarBind] -> [Type]
mkClassInst n tvs = [foldTyApp id $ reverse
                  $ (mkTyCon n) : (map mkTyVar $ genNames $ length tvs)]

mkDTName :: String -> String
mkDTName n = "dggDT_" ++ n

mkData :: TCInfo -> Decl
mkData (TCInfo n _ tvs vcs) = InstDecl srcLoc
    (map mkClassReq $ genNames $ length tvs) (mkUId "Data") (mkClassInst n tvs)
    [mkInsDecl (map mkToConstr vcs),
    mkInsDecl [Match srcLoc (Ident "dataTypeOf") [PWildCard] Nothing
                     (UnGuardedRhs (mkIdent $ mkDTName n)) bdecls],
    mkInsDecl (map mkGfoldl vcs),
    mkInsDecl $ mkGunfold vcs]

mkInsDecl :: [Match] -> InstDecl
mkInsDecl = InsDecl . FunBind

mkGunfold vcs = [Match srcLoc (Ident "gunfold")
    [mkPIdent "k", mkPIdent "z", mkPIdent "c"] Nothing
    (UnGuardedRhs (Case (App (mkIdent "constrIndex") (mkIdent "c"))
    ((map mkGunfoldAlt $ zip [1..] vcs) ++ [Alt srcLoc PWildCard (UnGuardedAlt
     (App (mkIdent "error") (Lit (String "gunfold: no match for ctor index"))))
      bdecls])
    )) bdecls]

mkGunfoldAlt :: (Int, VCInfo) -> Alt
mkGunfoldAlt (i, (VCInfo n a _ _ _ _ _)) = Alt srcLoc (PLit (Int (toInteger i))
    ) (UnGuardedAlt (foldApp id ((App (mkIdent "z") (mkCon n)) :
        (replicate a $ mkIdent "k")))) bdecls

mkToConstr :: VCInfo -> Match
mkToConstr (VCInfo n a _ _ _ _ _) = mkMatch "toConstr" [PApp (mkUId
    n) (replicate a PWildCard)] (mkIdent $ mkConstrName n)

mkGfoldl :: VCInfo -> Match
mkGfoldl (VCInfo n a _ _ _ _ _) = mkMatch "gfoldl" [mkPIdent "k", mkPIdent "z",
    PApp (mkUId n) (map mkPIdent $ genNames a)] (foldInApp (appInfix "k") id $
    reverse $ App (mkIdent "z") (mkCon n) : (map mkIdent $ genNames a))

mkDT (TCInfo n _ _ vcs) = PatBind srcLoc (mkPIdent $ mkDTName n) Nothing
    (UnGuardedRhs (App (App (mkIdent "mkDataType") (Lit (String n))) (List
    (map (mkIdent . mkConstrName . conName) vcs)))) bdecls

mkConstrName :: String -> String
mkConstrName n = "dggConstr_" ++ n

mkConstr tcn (VCInfo n _ _ _ _ _ _) = PatBind srcLoc (mkPIdent $ mkConstrName n)
    Nothing (UnGuardedRhs (App (App (App (App (mkIdent "mkConstr")
    (mkIdent $ mkDTName tcn)) (Lit (String n)))
    (List [])
--    (List [Lit (String "lTree"),Lit (String "bVal"),Lit (String "rTree")]) TODO: Record names
    ) (Con (UnQual(Ident "Prefix"))))) bdecls -- TODO: Fixity


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
