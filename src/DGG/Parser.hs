module DGG.Parser (
      module DGG.Adapter.EMGM
    , parseDTs
    ) where

import DGG.Data
import DGG.Adapter.EMGM
import Language.Haskell.Exts

parseDTs :: LibParser -> [Decl] -> [TCInfo]
parseDTs f = foldr (\x xs -> f x : xs) []

