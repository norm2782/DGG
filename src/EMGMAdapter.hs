{-# LANGUAGE TypeOperators #-}

--module GenGen.Adapter.EMGM where
module EMGMAdapter where
-- import GenGen.Representation
import Language.Haskell.Exts.Syntax
import Generics.EMGM as E
import Prelude as P
import Main

--tyRep = TypeDecl (SourceLoc "" 0 0) (Name "" "mytypename")
data Tree a = Leaf a | Branch (Tree a) (Tree a) | LBranch (Tree a) (Tree a) (Tree a)

data Foo = Bar | Baz | Bat

data Foo' = Bar'

ggFoo' :: EP Foo' Unit
ggFoo' = EP from' to'
    where
        from' Bar' = Unit
        to' Unit = Bar'

-- For your datatypes, you have to define your own instances as below:
--    Embedding projection pair (conversion between the datatype and a
--    structural generic representation):
ggEPTree :: EP (Tree t) (t :+: (Tree t :*: Tree t)
                           :+: (Tree t :*: Tree t :*: Tree t))
ggEPTree = EP from' to' where
  from' (Leaf a)        = L a
  from' (Branch a b)    = R (L (a :*: b))
  from' (LBranch a b c) = R (R (a :*: b :*: c))
  to' (L (a))                 = Leaf a
  to' (R (L (a :*: b)))       = Branch a b
  to' (R (R (a :*: b :*: c))) = LBranch a b c


--ggFoo :: EP Foo (Unit :+: (Unit :+: Unit))
ggFoo = EP from' to'
    where
        from' Bar = L Unit
        from' Baz = R (L Unit)
        from' Bat = R (R Unit)
        to' (L Unit) = Bar
        to' (R (L Unit)) = Baz
        to' (R (R Unit)) = Bat

-- Actual code

newPat :: TCInfo -> Decl
newPat tc@(TCInfo _ TyDataType _) = createDTEP   tc
newPat tc@(TCInfo _ TyNewType  _) = createNTEP   tc
newPat tc@(TCInfo _ TySynonym  _) = createSynEP  tc
newPat tc@(TCInfo _ TyGADT     _) = createGADTEP tc

srcLoc = SrcLoc "" 0 0

createDTEP (TCInfo tn TyDataType vcis) =
  PatBind srcLoc (PVar (Ident $ "dgg_" ++ tn)) Nothing rhs (bdecls vcis)



createNTEP   (TCInfo tn TyNewType  vcis) = undefined
createSynEP  (TCInfo tn TySynonym  vcis) = undefined
createGADTEP (TCInfo tn TyGADT     vcis) = undefined

fromFunName = "from'"
toFunName   = "to'"
unitType    = "Unit"

rhs = UnGuardedRhs (App (App (Con (UnQual (Ident "EP")))
                             (Var (UnQual (Ident fromFunName))))
                             (Var (UnQual (Ident toFunName))))

bdecls :: [VCInfo] -> Binds
bdecls vcis = BDecls $ (P.map bdeclFrom vcis) ++ (P.map bdeclTo vcis)

bdeclFrom :: VCInfo -> Decl
bdeclFrom (VCInfo n ar i f as rs) =
    FunBind [Match srcLoc (Ident fromFunName) 
        [PParen (PApp (UnQual (Ident n)) (P.map (PVar . Ident . recTyname) rs))]
        Nothing 
        (UnGuardedRhs (App (Con (UnQual (Ident "L"))) -- TODO: Dynamic L R etc
            (Paren (buildProd rs)))) (BDecls [])]

buildProd :: [Record] -> Exp            
buildProd rs = buildInApp $ reverse $ P.map recTyname rs

buildInApp :: [String] -> Exp
buildInApp (x:xs) = InfixApp (buildInApp xs) (QConOp (UnQual (Symbol ":*:"))) 
                                             (Var (UnQual (Ident x)))

{-
(UnGuardedRhs (App (Con (UnQual (Ident "R"))) (Paren (App (Con (UnQual (Ident
"R"))) (Paren (InfixApp (InfixApp (Var (UnQual (Ident "a"))) (QConOp (UnQual (
Symbol ":*:"))) (Var (UnQual (Ident "b")))) (QConOp (UnQual (Symbol ":*:"))) (
Var (UnQual (Ident "c"))))))))) 
-}

{-
PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 29, srcColumn = 1}) 
(PVar (Ident "ggEPTree")) Nothing (UnGuardedRhs (App (App (Con (UnQual (Ident 
"EP"))) (Var (UnQual (Ident "from'")))) (Var (UnQual (Ident "to'"))))) (BDecls
[FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 30, 
srcColumn = 3}) (Ident "from'") [PParen (PApp (UnQual (Ident "Leaf")) [PVar (
Ident "a")])] Nothing (UnGuardedRhs (App (Con (UnQual (Ident "L"))) (Var (
UnQual (Ident "a"))))) (BDecls []),Match (SrcLoc {srcFilename = 
"EMGMAdapter.hs", srcLine = 31, srcColumn = 3}) (Ident "from'") [PParen (PApp (
UnQual (Ident "Branch")) [PVar (Ident "a"),PVar (Ident "b")])] Nothing (
UnGuardedRhs (App (Con (UnQual (Ident "R"))) (Paren (App (Con (UnQual (Ident 
"L"))) (Paren (InfixApp (Var (UnQual (Ident "a"))) (QConOp (UnQual (Symbol 
":*:"))) (Var (UnQual (Ident "b"))))))))) (BDecls []),Match (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 32, srcColumn = 3}) (Ident "from'") [
PParen (PApp (UnQual (Ident "LBranch")) [PVar (Ident "a"),PVar (Ident "b"),PVar
(Ident "c")])] Nothing (UnGuardedRhs (App (Con (UnQual (Ident "R"))) (Paren (
App (Con (UnQual (Ident "R"))) (Paren (InfixApp (InfixApp (Var (UnQual (Ident 
"a"))) (QConOp (UnQual (Symbol ":*:"))) (Var (UnQual (Ident "b")))) (QConOp (
UnQual (Symbol ":*:"))) (Var (UnQual (Ident "c"))))))))) (BDecls [])],FunBind [
Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 33, srcColumn = 3}) (
Ident "to'") [PParen (PApp (UnQual (Ident "L")) [PParen (PVar (Ident "a"))])] 
Nothing (UnGuardedRhs (App (Con (UnQual (Ident "Leaf"))) (Var (UnQual (Ident 
"a"))))) (BDecls []),Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 
34, srcColumn = 3}) (Ident "to'") [PParen (PApp (UnQual (Ident "R")) [PParen (
PApp (UnQual (Ident "L")) [PParen (PInfixApp (PVar (Ident "a")) (UnQual (Symbol
":*:")) (PVar (Ident "b")))])])] Nothing (UnGuardedRhs (App (App (Con (UnQual (
Ident "Branch"))) (Var (UnQual (Ident "a")))) (Var (UnQual (Ident "b"))))) (
BDecls []),Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 35, 
srcColumn = 3}) (Ident "to'") [PParen (PApp (UnQual (Ident "R")) [PParen (PApp 
(UnQual (Ident "R")) [PParen (PInfixApp (PInfixApp (PVar (Ident "a")) (UnQual (
Symbol ":*:")) (PVar (Ident "b"))) (UnQual (Symbol ":*:")) (PVar (Ident "c")))]
)])] Nothing (UnGuardedRhs (App (App (App (Con (UnQual (Ident "LBranch"))) (Var
(UnQual (Ident "a")))) (Var (UnQual (Ident "b")))) (Var (UnQual (Ident "c")))))
(BDecls [])]])
-}

bdeclTo :: VCInfo -> Decl
bdeclTo vci = FunBind []



fromVCRep    (VCInfo cn 0 _ _ _ _) = getEPMatchA0 fromFunName cn unitType
toVCRepMatch (VCInfo cn 0 _ _ _ _) = getEPMatchA0 toFunName unitType cn

vcRep (VCInfo _ 1 _ _ _ _) = undefined

getEPMatchA0 :: String -> String -> String -> Match
getEPMatchA0 d l r = Match srcLoc (Ident d)
                        [PApp (UnQual (Ident l)) []]
                        Nothing
                        (UnGuardedRhs (Con (UnQual (Ident r))))
                        (BDecls [])

{-
Match srcLoc (Ident "from'")
    [PParen (PApp (UnQual (Ident "Leaf")) [PVar (Ident "aa")])]
    Nothing
    (UnGuardedRhs (App (Con (UnQual (Ident "L"))) (Paren (Var (UnQual (Ident "aa"))))))
    (BDecls []),
    
Match srcLoc (Ident "from'")
    [PParen (PApp (UnQual (Ident "Branch")) [PVar (Ident "aa"),PVar (Ident "ab")])]
    Nothing
    (UnGuardedRhs (App (Con (UnQual (Ident "R"))) (Paren (InfixApp (Var (UnQual (Ident "aa"))) (QConOp (UnQual (Symbol ":*:"))) (Paren (Var (UnQual (Ident "ab"))))))))
    (BDecls [])
-}
{-


if TC list of VC length = 0, then .. I don't know!
if TC list of VC length = 1, then do not use a sum
if TC list of VC length > 1, use a product

We need:
- A function to create a representiation for a value constructor
- A function to create a representation for an entire type, using the previous function.

if VC arity = 0, then use Unit
arity = 1 : use reptype for that particular item
arity > 1 : use a product of reptypes




--[C1, C2, C3]

toSum [] = []
toSum [x] = R
toSum (x:xs) = L : R : foo xs
-}
{-
DataDecl (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 12, srcColumn = 1})
DataType [] (Ident "Tree") [UnkindedVar (Ident "a")] [QualConDecl (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 12, srcColumn = 15}) [] [] (ConDecl (
Ident "Leaf") [UnBangedTy (TyVar (Ident "a"))]),QualConDecl (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 12, srcColumn = 24}) [] [] (ConDecl (
Ident "Branch") [UnBangedTy (TyParen (TyApp (TyCon (UnQual (Ident "Tree"))) (
TyVar (Ident "a")))),UnBangedTy (TyParen (TyApp (TyCon (UnQual (Ident "Tree")))
(TyVar (Ident "a"))))])] []

DataDecl (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 14, srcColumn = 1})
DataType [] (Ident "Foo") [] [QualConDecl (SrcLoc {srcFilename = 
"EMGMAdapter.hs", srcLine = 14, srcColumn = 12}) [] [] (ConDecl (Ident "Bar")
[]),QualConDecl (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 14,
srcColumn = 18}) [] [] (ConDecl (Ident "Baz") []),QualConDecl (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 14, srcColumn = 24}) [] [] (ConDecl (
Ident "Bat") [])] []

DataDecl (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 16, srcColumn = 1})
DataType [] (Ident "Foo'") [] [QualConDecl (SrcLoc {srcFilename = 
"EMGMAdapter.hs", srcLine = 16, srcColumn = 13}) [] [] (ConDecl (Ident "Bar'") 
[])] []

TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 18, srcColumn = 1})
[Ident "ggFoo'"] (TyApp (TyApp (TyCon (UnQual (Ident "EP"))) (TyCon (UnQual (
Ident "Foo'")))) (TyCon (UnQual (Ident "Unit"))))

PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 19, srcColumn = 1})
(PVar (Ident "ggFoo'")) Nothing (UnGuardedRhs (App (App (Con (UnQual (Ident 
"EP"))) (Var (UnQual (Ident "from'")))) (Var (UnQual (Ident "to'"))))) (BDecls
[FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 21, 
srcColumn = 9}) (Ident "from'") [PApp (UnQual (Ident "Bar'")) []] Nothing (
UnGuardedRhs (Con (UnQual (Ident "Unit")))) (BDecls [])],FunBind [Match (
SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 22, srcColumn = 9}) (Ident 
"to'") [PApp (UnQual (Ident "Unit")) []] Nothing (UnGuardedRhs (Con (UnQual (
Ident "Bar'")))) (BDecls [])]])

TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 27, srcColumn = 1})
[Ident "ggEPTree"] (TyApp (TyApp (TyCon (UnQual (Ident "EP"))) (TyParen (TyApp
(TyCon (UnQual (Ident "Tree"))) (TyVar (Ident "t"))))) (TyParen (TyInfix (TyVar
(Ident "t")) (UnQual (Symbol ":+:")) (TyParen (TyInfix (TyApp (TyCon (UnQual (
Ident "Tree"))) (TyVar (Ident "t"))) (UnQual (Symbol ":*:")) (TyApp (TyCon (
UnQual (Ident "Tree"))) (TyVar (Ident "t"))))))))

PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 28, srcColumn = 1})
(PVar (Ident "ggEPTree")) Nothing (UnGuardedRhs (App (App (Con (UnQual (Ident 
"EP"))) (Var (UnQual (Ident "from'")))) (Var (UnQual (Ident "to'"))))) (BDecls
[FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 29, 
srcColumn = 3}) (Ident "from'") [PParen (PApp (UnQual (Ident "Leaf")) [PVar (
Ident "aa")])] Nothing (UnGuardedRhs (App (Con (UnQual (Ident "L"))) (Paren (
Var (UnQual (Ident "aa")))))) (BDecls []),Match (SrcLoc {srcFilename = 
"EMGMAdapter.hs", srcLine = 30, srcColumn = 3}) (Ident "from'") [PParen (PApp (
UnQual (Ident "Branch")) [PVar (Ident "aa"),PVar (Ident "ab")])] Nothing (
UnGuardedRhs (App (Con (UnQual (Ident "R"))) (Paren (InfixApp (Var (UnQual (
Ident "aa"))) (QConOp (UnQual (Symbol ":*:"))) (Paren (Var (UnQual (Ident "ab")
))))))) (BDecls [])],FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", 
srcLine = 31, srcColumn = 3}) (Ident "to'") [PParen (PApp (UnQual (Ident "L"))
[PParen (PVar (Ident "aa"))])] Nothing (UnGuardedRhs (Paren (App (Con (UnQual (
Ident "Leaf"))) (Var (UnQual (Ident "aa")))))) (BDecls []),Match (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 32, srcColumn = 3}) (Ident "to'") [
PParen (PApp (UnQual (Ident "R")) [PParen (PInfixApp (PVar (Ident "aa")) (
UnQual (Symbol ":*:")) (PParen (PVar (Ident "ab"))))])] Nothing (UnGuardedRhs (
Paren (App (App (Con (UnQual (Ident "Branch"))) (Var (UnQual (Ident "aa")))) (
Var (UnQual (Ident "ab")))))) (BDecls [])]])

PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 36, srcColumn = 1})
(PVar (Ident "ggFoo")) Nothing (UnGuardedRhs (App (App (Con (UnQual (Ident "EP"
))) (Var (UnQual (Ident "from'")))) (Var (UnQual (Ident "to'"))))) (BDecls [
FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 38, srcColumn
= 9}) (Ident "from'") [PApp (UnQual (Ident "Bar")) []] Nothing (UnGuardedRhs (
App (Con (UnQual (Ident "L"))) (Con (UnQual (Ident "Unit"))))) (BDecls []),
Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 39, srcColumn = 9}) (
Ident "from'") [PApp (UnQual (Ident "Baz")) []] Nothing (UnGuardedRhs (App (Con
(UnQual (Ident "R"))) (Paren (App (Con (UnQual (Ident "L"))) (Con (UnQual (
Ident "Unit"))))))) (BDecls []),Match (SrcLoc {srcFilename = "EMGMAdapter.hs", 
srcLine = 40, srcColumn = 9}) (Ident "from'") [PApp (UnQual (Ident "Bat")) []] 
Nothing (UnGuardedRhs (App (Con (UnQual (Ident "R"))) (Paren (App (Con (UnQual 
(Ident "R"))) (Con (UnQual (Ident "Unit"))))))) (BDecls [])],FunBind [Match (
SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 41, srcColumn = 9}) (Ident 
"to'") [PParen (PApp (UnQual (Ident "L")) [PApp (UnQual (Ident "Unit")) []])] 
Nothing (UnGuardedRhs (Con (UnQual (Ident "Bar")))) (BDecls []),Match (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 42, srcColumn = 9}) (Ident "to'") [
PParen (PApp (UnQual (Ident "R")) [PParen (PApp (UnQual (Ident "L")) [PApp (
UnQual (Ident "Unit")) []])])] Nothing (UnGuardedRhs (Con (UnQual (Ident "Baz")
))) (BDecls []),Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 43, 
srcColumn = 9}) (Ident "to'") [PParen (PApp (UnQual (Ident "R")) [PParen (PApp 
(UnQual (Ident "R")) [PApp (UnQual (Ident "Unit")) []])])] Nothing (
UnGuardedRhs (Con (UnQual (Ident "Bat")))) (BDecls [])]])

TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 47, srcColumn = 1}) 
[Ident "newPat"] (TyFun (TyCon (UnQual (Ident "TCInfo"))) (TyCon (UnQual (Ident
"Decl"))))

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 48, srcColumn
= 1}) (Ident "newPat") [PAsPat (Ident "tc") (PParen (PApp (UnQual (Ident 
"TCInfo")) [PWildCard,PApp (UnQual (Ident "TyDataType")) [],PWildCard]))]
Nothing (UnGuardedRhs (App (Var (UnQual (Ident "createDTEP"))) (Var (UnQual (
Ident "tc"))))) (BDecls []),Match (SrcLoc {srcFilename = "EMGMAdapter.hs", 
srcLine = 49, srcColumn = 1}) (Ident "newPat") [PAsPat (Ident "tc") (PParen (
PApp (UnQual (Ident "TCInfo")) [PWildCard,PApp (UnQual (Ident "TyNewType")) [],
PWildCard]))] Nothing (UnGuardedRhs (App (Var (UnQual (Ident "createNTEP"))) (
Var (UnQual (Ident "tc"))))) (BDecls []),Match (SrcLoc {srcFilename = 
"EMGMAdapter.hs", srcLine = 50, srcColumn = 1}) (Ident "newPat") [PAsPat (Ident
"tc") (PParen (PApp (UnQual (Ident "TCInfo")) [PWildCard,PApp (UnQual (Ident 
"TySynonym")) [],PWildCard]))] Nothing (UnGuardedRhs (App (Var (UnQual (Ident 
"createSynEP"))) (Var (UnQual (Ident "tc"))))) (BDecls []),Match (SrcLoc {
srcFilename = "EMGMAdapter.hs", srcLine = 51, srcColumn = 1}) (Ident "newPat") 
[PAsPat (Ident "tc") (PParen (PApp (UnQual (Ident "TCInfo")) [PWildCard,PApp (
UnQual (Ident "TyGADT")) [],PWildCard]))] Nothing (UnGuardedRhs (App (Var (
UnQual (Ident "createGADTEP"))) (Var (UnQual (Ident "tc"))))) (BDecls [])]

PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 53, srcColumn = 1})
(PVar (Ident "srcLoc")) Nothing (UnGuardedRhs (App (App (App (Con (UnQual (
Ident "SrcLoc"))) (Lit (String ""))) (Lit (Int 0))) (Lit (Int 0)))) (BDecls [])

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 55, srcColumn
= 1}) (Ident "createDTEP") [PParen (PApp (UnQual (Ident "TCInfo")) [PVar (Ident
"tn"),PApp (UnQual (Ident "TyDataType")) [],PVar (Ident "vcis")])] Nothing (
UnGuardedRhs (App (App (App (App (App (Con (UnQual (Ident "PatBind"))) (Var (
UnQual (Ident "srcLoc")))) (Paren (App (Con (UnQual (Ident "PVar"))) (Paren (
InfixApp (Con (UnQual (Ident "Ident"))) (QVarOp (UnQual (Symbol "$"))) (
InfixApp (Lit (String "dgg_")) (QVarOp (UnQual (Symbol "++"))) (Var (UnQual (
Ident "tn"))))))))) (Con (UnQual (Ident "Nothing")))) (Var (UnQual (Ident "rhs"
)))) (Paren (App (Var (UnQual (Ident "bdecls"))) (Var (UnQual (Ident "vcis"))))
))) (BDecls [])]

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 60, srcColumn
= 1}) (Ident "createNTEP") [PParen (PApp (UnQual (Ident "TCInfo")) [PVar (Ident
"tn"),PApp (UnQual (Ident "TyNewType")) [],PVar (Ident "vcis")])] Nothing (
UnGuardedRhs (Var (UnQual (Ident "undefined")))) (BDecls [])]

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 61, srcColumn
= 1}) (Ident "createSynEP") [PParen (PApp (UnQual (Ident "TCInfo")) [PVar (
Ident "tn"),PApp (UnQual (Ident "TySynonym")) [],PVar (Ident "vcis")])] Nothing
(UnGuardedRhs (Var (UnQual (Ident "undefined")))) (BDecls [])]

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 62, srcColumn
= 1}) (Ident "createGADTEP") [PParen (PApp (UnQual (Ident "TCInfo")) [PVar (
Ident "tn"),PApp (UnQual (Ident "TyGADT")) [],PVar (Ident "vcis")])] Nothing (
UnGuardedRhs (Var (UnQual (Ident "undefined")))) (BDecls [])]

PatBind (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 65, srcColumn = 1}) 
(PVar (Ident "rhs")) Nothing (UnGuardedRhs (App (Con (UnQual (Ident 
"UnGuardedRhs"))) (Paren (App (App (Con (UnQual (Ident "App"))) (Paren (App (
App (Con (UnQual (Ident "App"))) (Paren (App (Con (UnQual (Ident "Con"))) (
Paren (App (Con (UnQual (Ident "UnQual"))) (Paren (App (Con (UnQual (Ident 
"Ident"))) (Lit (String "EP"))))))))) (Paren (App (Con (UnQual (Ident "Var")))
(Paren (App (Con (UnQual (Ident "UnQual"))) (Paren (App (Con (UnQual (Ident 
"Ident"))) (Lit (String "from'"))))))))))) (Paren (App (Con (UnQual (Ident 
"Var"))) (Paren (App (Con (UnQual (Ident "UnQual"))) (Paren (App (Con (UnQual (
Ident "Ident"))) (Lit (String "to'")))))))))))) (BDecls [])

TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 69, srcColumn = 1}) 
[Ident "bdecls"] (TyFun (TyList (TyCon (UnQual (Ident "VCInfo")))) (TyCon (
UnQual (Ident "Binds"))))

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 70, srcColumn
= 1}) (Ident "bdecls") [PVar (Ident "vcis")] Nothing (UnGuardedRhs (InfixApp (
Con (UnQual (Ident "BDecls"))) (QVarOp (UnQual (Symbol "$"))) (List [App (Var (
UnQual (Ident "bdeclFrom"))) (Var (UnQual (Ident "vcis"))),App (Var (UnQual (
Ident "bdeclTo"))) (Var (UnQual (Ident "vcis")))]))) (BDecls [])]

TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 72, srcColumn = 1}) 
[Ident "bdeclFrom"] (TyFun (TyList (TyCon (UnQual (Ident "VCInfo")))) (TyCon (
UnQual (Ident "Decl"))))

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 73, srcColumn
= 1}) (Ident "bdeclFrom") [PVar (Ident "vci")] Nothing (UnGuardedRhs (App (Con
(UnQual (Ident "FunBind"))) (List [App (App (App (App (App (App (Con (UnQual (
Ident "Match"))) (Var (UnQual (Ident "srcLoc")))) (Paren (App (Con (UnQual (
Ident "Ident"))) (Lit (String "from'"))))) (List [App (Con (UnQual (Ident 
"PParen"))) (Paren (App (App (Con (UnQual (Ident "PApp"))) (Paren (App (Con (
UnQual (Ident "UnQual"))) (Paren (App (Con (UnQual (Ident "Ident"))) (Lit (
String "Leaf"))))))) (List [App (Con (UnQual (Ident "PVar"))) (Paren (App (Con
(UnQual (Ident "Ident"))) (Lit (String "aa"))))])))])) (Con (UnQual (Ident 
"Nothing")))) (Paren (App (Con (UnQual (Ident "UnGuardedRhs"))) (Paren (App (
App (Con (UnQual (Ident "App"))) (Paren (App (Con (UnQual (Ident "Con"))) (
Paren (App (Con (UnQual (Ident "UnQual"))) (Paren (App (Con (UnQual (Ident 
"Ident"))) (Lit (String "L"))))))))) (Paren (App (Con (UnQual (Ident "Paren")))
(Paren (App (Con (UnQual (Ident "Var"))) (Paren (App (Con (UnQual (Ident 
"UnQual"))) (Paren (App (Con (UnQual (Ident "Ident"))) (Lit (String "aa")))))))
)))))))) (Paren (App (Con (UnQual (Ident "BDecls"))) (List [])))]))) (BDecls []
)]

TypeSig (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 81, srcColumn = 1})
[Ident "bdeclTo"] (TyFun (TyList (TyCon (UnQual (Ident "VCInfo")))) (TyCon (
UnQual (Ident "Decl"))))

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 82, srcColumn
= 1}) (Ident "bdeclTo") [PVar (Ident "vci")] Nothing (UnGuardedRhs (App (Con (
UnQual (Ident "FunBind"))) (List []))) (BDecls [])]

FunBind [Match (SrcLoc {srcFilename = "EMGMAdapter.hs", srcLine = 86, srcColumn
= 1}) (Ident "vcRep") [PParen (PApp (UnQual (Ident "VCInfo")) [PWildCard,PLit (
Int 0),PWildCard,PWildCard,PWildCard,PWildCard])] Nothing (UnGuardedRhs (App (
Con (UnQual (Ident "Ident"))) (Lit (String "Unit")))) (BDecls []),Match (SrcLoc
{srcFilename = "EMGMAdapter.hs", srcLine = 87, srcColumn = 1}) (Ident "vcRep")
[PParen (PApp (UnQual (Ident "VCInfo")) [PWildCard,PLit (Int 1),PWildCard,
PWildCard,PWildCard,PWildCard])] Nothing (UnGuardedRhs (Var (UnQual (Ident 
"undefined")))) (BDecls [])]

-}
