{-# LANGUAGE TypeOperators #-}

--module GenGen.Adapter.EMGM where
module EMGMAdapter where
-- import GenGen.Representation
import Language.Haskell.Exts.Syntax
import Generics.EMGM as E
import Prelude as P
import Main

--tyRep = TypeDecl (SourceLoc "" 0 0) (Name "" "mytypename")
data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- For your datatypes, you have to define your own instances as below:
--    Embedding projection pair (conversion between the datatype and a
--    structural generic representation):
ggEPTree :: EP (Tree t) (t :+: (Tree t :*: Tree t))
ggEPTree = EP from' to' where
  from' (Leaf aa) = L (aa)
  from' (Branch aa ab) = R (aa :*: (ab))
  to' (L (aa)) = (Leaf aa)
  to' (R (aa :*: (ab))) = (Branch aa ab)



-- Actual code

newPat :: TCInfo -> Decl
newPat tc@(TCInfo _ TyDataType _) = createDTEP   tc
newPat tc@(TCInfo _ TyNewType  _) = createNTEP   tc
newPat tc@(TCInfo _ TySynonym  _) = createSynEP  tc
newPat tc@(TCInfo _ TyGADT     _) = createGADTEP tc

srcLoc = SrcLoc "" 0 0

createDTEP (TCInfo tn TyDataType vcis) =
  PatBind srcLoc (PVar (Ident $ "gg" ++ tn)) Nothing rhs (bdecls vcis)



createNTEP   (TCInfo tn TyNewType  vcis) = undefined
createSynEP  (TCInfo tn TySynonym  vcis) = undefined
createGADTEP (TCInfo tn TyGADT     vcis) = undefined


rhs = UnGuardedRhs (App (App (Con (UnQual (Ident "EP")))
                             (Var (UnQual (Ident "from'"))))
                             (Var (UnQual (Ident "to'"))))

bdecls :: [VCInfo] -> Binds
bdecls vcis = BDecls $ [bdeclFrom vcis, bdeclTo vcis]

bdeclFrom :: [VCInfo] -> Decl
bdeclFrom vci =
-- TODO: Make dynamic
    FunBind [Match srcLoc (Ident "from'") 
                [PParen (PApp (UnQual (Ident "Leaf")) [PVar (Ident "aa")])]
                Nothing (UnGuardedRhs
                    (App (Con (UnQual (Ident "L")))
                         (Paren (Var (UnQual (Ident "aa")))))) (BDecls [])]

bdeclTo :: [VCInfo] -> Decl
bdeclTo vci = FunBind []

{-
DataDecl (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 9, srcColumn = 1})
    DataType [] (Ident "Tree") [UnkindedVar (Ident "a")] [QualConDecl 
        (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 9, srcColumn = 15})
        [] [] (ConDecl (Ident "Leaf") [UnBangedTy (TyVar (Ident "a"))]),
        QualConDecl (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 9, 
        srcColumn = 24}) [] [] (ConDecl (Ident "Branch") [UnBangedTy (TyParen 
        (TyApp (TyCon (UnQual (Ident "Tree"))) (TyVar (Ident "a")))),UnBangedTy
        (TyParen (TyApp (TyCon (UnQual (Ident "Tree"))) (TyVar (Ident "a"))))])]
        []


TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 16, srcColumn = 1})
    [Ident "ggEPTree"] (TyApp (TyApp (TyCon (UnQual (Ident "EP"))) (TyParen
    (TyApp (TyCon (UnQual (Ident "Tree"))) (TyVar (Ident "t"))))) (TyParen
    (TyInfix (TyVar (Ident "t")) (UnQual (Symbol ":+:")) (TyParen (TyInfix
    (TyApp (TyCon (UnQual (Ident "Tree"))) (TyVar (Ident "t"))) (UnQual
    (Symbol ":*:")) (TyApp (TyCon (UnQual (Ident "Tree"))) (TyVar (Ident "t")))
    )))))

PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 17, srcColumn = 1})
    (PVar (Ident "ggEPTree")) Nothing (UnGuardedRhs (App (App (Con (UnQual
    (Ident "EP"))) (Var (UnQual (Ident "from'")))) (Var (UnQual (Ident "to'"))
    )))
    
    (BDecls [

        FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", 
    srcLine = 18, srcColumn = 3}) (Ident "from'")
        [PParen (PApp (UnQual (Ident
    "Leaf")) [PVar (Ident "aa")])]
        Nothing
        (UnGuardedRhs (App (Con (UnQual 
    (Ident "L"))) (Paren (Var (UnQual (Ident "aa")))))) (BDecls []),Match 
    (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 19, srcColumn = 3}) 
    (Ident "from'") [PParen (PApp (UnQual (Ident "Branch")) [PVar (Ident "aa"),
    PVar (Ident "ab")])] Nothing (UnGuardedRhs (App (Con (UnQual (Ident "R")))
    (Paren (InfixApp (Var (UnQual (Ident "aa"))) (QConOp (UnQual (Symbol ":*:")
    )) (Paren (Var (UnQual (Ident "ab")))))))) (BDecls [])],

        FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 20,
    srcColumn = 3}) (Ident "to'") [PParen (PApp (UnQual (Ident "L")) [PParen
    (PVar (Ident "aa"))])] Nothing (UnGuardedRhs (Paren (App (Con (UnQual
    (Ident "Leaf"))) (Var (UnQual (Ident "aa")))))) (BDecls []),Match (SrcLoc
    {srcFilename = "EMGMAdapter.hs", srcLine = 21, srcColumn = 3}) (Ident "to'"
    ) [PParen (PApp (UnQual (Ident "R")) [PParen (PInfixApp (PVar (Ident "aa"))
    (UnQual (Symbol ":*:")) (PParen (PVar (Ident "ab"))))])] Nothing
    (UnGuardedRhs (Paren (App (App (Con (UnQual (Ident "Branch"))) (Var (UnQual
    (Ident "aa")))) (Var (UnQual (Ident "ab")))))) (BDecls [])]])

-}

{-
s = PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 13, srcColumn = 1})
            (PVar (Ident "epTree")) Nothing
            (UnGuardedRhs (App (App (Con (UnQual (Ident "EP")))
                                    (Var (UnQual (Ident "from'"))))
                                    (Var (UnQual (Ident "to'")))))
            (BDecls [FunBind
                        [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 14, srcColumn = 3})
                               (Ident "from'")
                               [PParen (PApp (UnQual (Ident "Leaf")) [PVar (Ident "aa")])]
                               Nothing 
                               (UnGuardedRhs 
                                    (App (Con (UnQual (Ident "L"))) (Paren (Var (UnQual (Ident "aa"))))))
                                    (BDecls []),
                                    Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 15, srcColumn = 3})
                                    (Ident "from'")
                                    [PParen (PApp (UnQual (Ident "Branch"))
                                    [PVar (Ident "aa"),PVar (Ident "ab")])]
                                    Nothing
                                    (UnGuardedRhs (App (Con (UnQual (Ident "R")))
                                                       (Paren (InfixApp (Var (UnQual (Ident "aa"))) (QConOp (UnQual (Symbol ":*:"))) (Paren (Var (UnQual (Ident "ab")))))))) (BDecls [])],FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 16, srcColumn = 3}) (Ident "to'") [PParen (PApp (UnQual (Ident "L")) [PParen (PVar (Ident "aa"))])] Nothing (UnGuardedRhs (Paren (App (Con (UnQual (Ident "Leaf"))) (Var (UnQual (Ident "aa")))))) (BDecls []),Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 17, srcColumn = 3}) (Ident "to'") [PParen (PApp (UnQual (Ident "R")) [PParen (PInfixApp (PVar (Ident "aa")) (UnQual (Symbol ":*:")) (PParen (PVar (Ident "ab"))))])] Nothing (UnGuardedRhs (Paren (App (App (Con (UnQual (Ident "Branch"))) (Var (UnQual (Ident "aa")))) (Var (UnQual (Ident "ab")))))) (BDecls [])]])
-}
