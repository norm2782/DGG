module DGG.Parser (
      module DGG.Adapter.EMGM
    , module DGG.Adapter.MultiRec
    , module DGG.Adapter.SYB
    , parseDTs
    ) where

import DGG.Data
import DGG.Adapter.EMGM
import DGG.Adapter.MultiRec
import DGG.Adapter.SYB
import Language.Haskell.Exts

parseDTs :: LibParser -> [Decl] -> [TCInfo]
parseDTs f = foldr (\x xs -> f x : xs) []

