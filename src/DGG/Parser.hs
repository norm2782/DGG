module DGG.Parser where

import DGG.Data
import Language.Haskell.Exts

parse :: String -> [Decl] -> [TCInfo]
parse a xs = foldr mkTCI [] xs

mkTCI :: Decl -> [TCInfo] -> [TCInfo]
mkTCI d tcis = d2tci d : tcis

d2tci :: Decl -> TCInfo
d2tci d = undefined
