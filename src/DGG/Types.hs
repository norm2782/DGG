{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}


module DGG.Types where

-- General imports
import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils

-- GHC related imports
import GHC hiding (Kind, Fixity)
import HsTypes

data DataType name = DataType {
     tyCon  :: TypeCon
  ,  ctors  :: [Ctor name]
}

type TName = String

-- Stolen from HSE
data Kind
    = KindStar
    | KindBang
    | KindFn Kind Kind
    | KindParen Kind
    | KindVar TName

data TypeCon = TypeCon {
     tyConName  :: TName
  ,  tyKind     :: Kind
  ,  tyVars     :: [TName]
}

-- Stolen from HSE
data Assoc
     = AssocNone
     | AssocLeft
     | AssocRight

-- Stolen from HSE
data Fixity = Fixity Assoc Int

data Ctor name = Ctor {
     ctorName    :: CName
  ,  ctorFixity  :: Fixity
  ,  ctorFields  :: Fields name
}

data Fields name = Fields [HsType name]
                 | RecordFields [(CName, HsType name)]

data CName = Ident String (Maybe String)
           | Symbol String String

{- data EMGM-}
{- data MultiRec-}

{- data Lib :: * -> * where-}
  {- EMGMLib :: Lib EMGM-}
  {- MultiRecLib :: Lib MultiRec-}

{- class Adapter a where-}
  {- gencode     :: Lib a -> [DataType name] -> m [HsDecl name]-}
  {- imports     :: Lib a -> [String]-}
  {- extensions  :: Lib a -> [String]-}

{- instance Adapter EMGM where-}
  {- gencode     _ = undefined-}
  {- imports     _ = []-}
  {- extensions  _ = []-}

{- instance Adapter MultiRec where-}
  {- gencode     _ = undefined-}
  {- imports     _ = []-}
  {- extensions  _ = []-}

{- parseAdapter :: Parser (Lib a)-}
{- parseAdapter = pEMGM <|> pMultiRec-}

{- pEMGM :: Parser (Lib a)-}
{- pEMGM = EMGMLib <$ pSymbol "emgm"-}

{- pMultiRec :: Parser (Lib a)-}
{- pMultiRec = MultiRecLib <$ pSymbol "multirec"-}


{- startParse :: Parser a -> String ->  (a, [Error LineColPos])-}
{- startParse p inp  =  parse ((,) <$> p <*> pEnd)-}
                  {- $  createStr (LineColPos 0 0 0) inp-}
