module DGG.Adapter where

import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax

srcLoc :: SrcLoc
srcLoc = SrcLoc "" 0 0

mkIdent :: String -> Exp
mkIdent = Var . unQualIdent 

unQualIdent :: String -> QName
unQualIdent = UnQual . Ident

conUnQualIdent :: String -> Exp
conUnQualIdent = Con . unQualIdent

pVarIdent :: String -> Pat
pVarIdent = PVar . Ident

pAppConUnQualIdent :: String -> Exp -> Exp
pAppConUnQualIdent s e = Paren (App (conUnQualIdent s) e)

mkPIdent :: String -> Pat
mkPIdent = PVar . Ident 

genNames :: Int -> [String]
genNames n = take n genNames'

genNames' :: [String]
genNames' = map (\x -> 'a' : show x) [1..]

unQualSym :: String -> QName
unQualSym = UnQual . Symbol

