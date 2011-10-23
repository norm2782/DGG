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

data DataType = DataType {
     tyCon  :: TypeCon
  ,  ctors  :: [Con]
}

type TName = String

data Kind
    = KindStar
    | KindFun Kind Kind

data TypeCon = TypeCon {
     tyConName  :: TName
  ,  tyKind     :: Kind
  ,  tyVars     :: [TName]
}

data Assoc
     = AssocNone
     | AssocLeft
     | AssocRight

data Fixity = Fixity Assoc Int

data Con = Con {
     conName    :: CName
  ,  conFixity  :: Fixity
  ,  conFields  :: Fields
}

type Type = String

data Fields = Fields [Type]
            | RecordFields [(CName, Type)]

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
