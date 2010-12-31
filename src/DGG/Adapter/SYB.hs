module DGG.Adapter.SYB (
      makeSYB
    , isSuppSYB
    , importsSYB
    ) where

import DGG.Data
import DGG.Adapter
import Language.Haskell.Exts.Syntax

importsSYB :: [ImportDecl]
importsSYB = [mkImport "Data.Data", mkImport "Data.Generics"]

makeSYB :: LibParser
makeSYB tc@(TCInfo _ TyDataType _ _) = [mkTypeable tc, mkData tc]

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

mkTypeable :: TCInfo -> Decl
mkTypeable (TCInfo n _ tvs vcs) = InstDecl srcLoc [] (mkUId "Typeable")
    [foldTyApp id $ reverse $ (mkTyCon n) : (map mkTyVar $ genNames $ length tvs)]
    [InsDecl (FunBind [Match srcLoc (Ident "typeOf") [PWildCard] Nothing (
    UnGuardedRhs (App (App (mkIdent "mkTyConApp") (App (mkIdent "mkTyCon") (Lit
    (String n)))) (List []))) bdecls])]

mkData :: TCInfo -> Decl
mkData (TCInfo n _ tvs vcs) = InstDecl srcLoc
    [ClassA (mkUId "Data") [mkTyVar "a"]]
    (mkUId "Data") [TyParen (TyApp (mkTyCon "MyTree") (mkTyVar "a"))] 
    [InsDecl (FunBind [Match srcLoc (Ident "toConstr") [PApp (mkUId "MyLeaf")
    []] Nothing (UnGuardedRhs (mkIdent "myLeafConstr")) (BDecls []),Match
    srcLoc (Ident "toConstr") [PParen (PApp (mkUId "MyBinNode") [PWildCard,
    PWildCard,PWildCard])] Nothing (UnGuardedRhs (mkIdent "myBinNConstr"))
    bdecls,Match srcLoc (Ident "toConstr") [PParen (PApp (mkUId "MyRTree")
    [PWildCard,PWildCard])] Nothing (UnGuardedRhs (mkIdent "myRTreeConstr"))
    bdecls]),
    InsDecl (FunBind [Match srcLoc (Ident "dataTypeOf") [PWildCard]
    Nothing (UnGuardedRhs (mkIdent "myTreeDT")) bdecls]),
    InsDecl (FunBind $ map mkGfoldl vcs)
    {-
    [Match srcLoc (Ident "gfoldl") [mkPIdent "k", mkPIdent "z",PApp (mkUId
    "MyLeaf") []] Nothing (UnGuardedRhs (App (mkIdent "z") (mkCon "MyLeaf")))
    bdecls,
    Match srcLoc (Ident "gfoldl") [mkPIdent "k", mkPIdent "z",PParen (
    PApp (mkUId "MyBinNode") [mkPIdent "t1", mkPIdent "a", mkPIdent "t2"])]
    Nothing (UnGuardedRhs (InfixApp (InfixApp (InfixApp (App (mkIdent "z")
    (mkCon "MyBinNode")) (appInfix "k") (mkIdent "t1")) (appInfix "k") (mkIdent
    "a")) (appInfix "k") (mkIdent "t2"))) bdecls,
    Match srcLoc (Ident "gfoldl")
    [mkPIdent "k", mkPIdent "z", PParen (PApp (mkUId "MyRTree") [mkPIdent "a",
    mkPIdent "t"])] Nothing (UnGuardedRhs (InfixApp (InfixApp (App (mkIdent "z"
    ) (mkCon "MyRTree")) (appInfix "k") (mkIdent "a")) (appInfix "k") (mkIdent
    "t"))) bdecls])
    -}
    ,
    InsDecl (FunBind [Match srcLoc (Ident "gunfold") [mkPIdent
    "k", mkPIdent "z", mkPIdent "c"] Nothing (UnGuardedRhs (Case (App (mkIdent
    "constrIndex") (mkIdent "c")) [Alt srcLoc (PLit (Int 1)) (UnGuardedAlt (App
    (mkIdent "z") (mkCon "MyLeaf"))) bdecls,Alt srcLoc (PLit (Int 2)) (
    UnGuardedAlt (App (mkIdent "k") (Paren (App (mkIdent "k") (Paren (App (
    mkIdent "k") (Paren (App (mkIdent "z") (mkCon "MyBinNode"))))))))) (BDecls
    []),Alt srcLoc (PLit (Int 3)) (UnGuardedAlt (Paren (App (mkIdent "k") (
    Paren (App (mkIdent "k") (Paren (App (mkIdent "z") (mkCon "MyRTree"))))))))
    bdecls,Alt srcLoc PWildCard (UnGuardedAlt (App (mkIdent "error") (Lit (
    String "")))) (BDecls [])])) bdecls])]

mkGfoldl :: VCInfo -> Match
mkGfoldl (VCInfo n a _ _ _ _ _) = Match srcLoc (Ident "gfoldl")
    [mkPIdent "k", mkPIdent "z", PApp (mkUId n) (map mkPIdent $ genNames a)]
    Nothing (UnGuardedRhs (foldInApp (appInfix "k") id $ reverse $
    App (mkIdent "z") (mkCon n) : (map mkIdent $ genNames a))) bdecls

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
