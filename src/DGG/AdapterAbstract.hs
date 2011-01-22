module DGG.AdapterAbstract (
      srcLoc, mkStrCon, mkNCon, mkIdent, mkUId, mkPIdent, genNames, unQualSym
    , mkImport, appInfix, bdecls, mkTyCon, mkTyVar, foldInApp, foldPInApp
    , foldXInApp, foldApp, foldTyApp, foldXApp, mkMatch, fromName, mkStrLit
    , deriveLib
    , module Data.Derive.Internal.Derivation
    , module DGG.Data
    , module DGG.Parser
    , module Language.Haskell
    ) where

import Data.Derive.Internal.Derivation
import DGG.Data
import DGG.Parser
import Language.Haskell hiding (genNames)

srcLoc :: SrcLoc
srcLoc = SrcLoc "" 0 0

mkStrCon :: String -> Exp
mkStrCon = Con . mkUId

mkNCon :: Name -> Exp
mkNCon = Con . UnQual

mkIdent :: String -> Exp
mkIdent = Var . mkUId 

mkUId :: String -> QName
mkUId = UnQual . Ident

mkPIdent :: String -> Pat
mkPIdent = PVar . Ident 

genNames :: Int -> [String]
genNames n = take n genNames'

genNames' :: [String]
genNames' = map (\x -> 'a' : show x) [1..]

unQualSym :: String -> QName
unQualSym = UnQual . Symbol

mkImport :: String -> ImportDecl
mkImport n = ImportDecl srcLoc (ModuleName n) False
                        False Nothing Nothing Nothing

appInfix :: String -> QOp
appInfix = QVarOp . mkUId

bdecls :: Binds
bdecls = BDecls []

mkTyCon :: Name -> Type
mkTyCon = TyCon . UnQual

mkTyVar :: String -> Type
mkTyVar = TyVar . Ident

foldInApp :: QOp -> (a -> Exp) -> [a] -> Exp
foldInApp = foldXInApp InfixApp

foldPInApp :: QName -> (a -> Pat) -> [a] -> Pat
foldPInApp = foldXInApp PInfixApp

foldXInApp :: (b -> c -> b -> b) -> c -> (a -> b) -> [a] -> b
foldXInApp _ _  mk [x] = mk x
foldXInApp c op mk (x:xs) = c (foldXInApp c op mk xs) op (mk x)

foldApp :: (a -> Exp) -> [a] -> Exp
foldApp = foldXApp App

foldTyApp :: (a -> Type) -> [a] -> Type
foldTyApp = foldXApp TyApp

-- TODO: Get rid of mk and replace by regular fold?
foldXApp :: (b -> b -> b) -> (a -> b) -> [a] -> b
foldXApp _ mk [x]    = mk x
foldXApp c mk (x:xs) = c (foldXApp c mk xs) (mk x)

mkMatch :: String -> [Pat] -> Exp -> Match
mkMatch n ps rhs = Match srcLoc (Ident n) ps Nothing (UnGuardedRhs rhs) bdecls

fromName :: Name -> String
fromName (Ident n)  = n
fromName (Symbol n) = n

mkStrLit :: Name -> Exp
mkStrLit n = (Lit . String) $ fromName n

deriveLib :: String -> CodeGenerator -> Derivation
deriveLib n cg = derivationCustom ("DGG.Adapter." ++ n ++ ".Derivation") $ mkFullDecl cg

mkFullDecl :: CodeGenerator -> FullDataDecl -> Either String [Decl]
mkFullDecl cg (_, decl) = Right $ (cg . mkTCI) decl
