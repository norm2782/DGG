module DGG.Adapter where

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax

srcLoc :: SrcLoc
srcLoc = SrcLoc "" 0 0

mkCon :: String -> Exp
mkCon = Con . unQualIdent

mkIdent :: String -> Exp
mkIdent = Var . unQualIdent 

unQualIdent :: String -> QName
unQualIdent = UnQual . Ident

conUnQualIdent :: String -> Exp
conUnQualIdent = Con . unQualIdent

pAppConUnQualIdent :: String -> Exp -> Exp
pAppConUnQualIdent = App . conUnQualIdent

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
appInfix n = QVarOp (unQualIdent n)
