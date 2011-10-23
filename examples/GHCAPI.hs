module GHCAPI where

import Control.Monad
import Data.Maybe

import DynFlags
import GHC
import GHC.Paths ( libdir )
import MonadUtils
import OccName
import Outputable
import RdrName


-- DGG Imports
import qualified DGG.Types as DGG

targetFile :: String
targetFile = "examples/testmodule.hs"

main :: IO ()
main = do
   res <- example
   return ()
   {- putStrLn "parsedSource: "-}
   {- putStrLn $ showSDoc ( ppr res )-}

example :: IO ParsedSource
example =
  defaultErrorHandler defaultLogAction $
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl xopt_set dflags
                          [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
      setSessionDynFlags dflags'
      target <- guessTarget targetFile Nothing
      setTargets [target]
      load LoadAllTargets
      modSum <- getModSummary $ mkModuleName "TestModule"
      p <- parseModule modSum
      t <- typecheckModule p
      {- d <- desugarModule t-}
      {- l <- loadModule d-}
      {- n <- getNamesInScope-}
      {- c <- return $ coreModule d-}
      {- g <- getModuleGraph-}
      {- mapM showModule g-}
      let ps = parsedSource t
      ts <- getTyDecls ps
      mapM_ toDGG ts
      {- liftIO $ putStrLn "getTys:"-}
      {- liftIO $ print ts-}
      return ps--,"\n-----\n",  typecheckedSource d)

getTyDecls :: Monad m => ParsedSource -> m [TyClDecl RdrName]
getTyDecls ps = return [d | TyClD d <- map unLoc (hsmodDecls $ unLoc ps)]
-- TODO: Filter datatypes based on a user-provided whitelist (or just take all
-- of them when no whitelist is provided)


newtype GHCDataDecl = GHCDataDecl (TyClDecl RdrName)
newtype GHCTySynDecl = GHCTySynDecl (TyClDecl RdrName)

toDGG :: Monad m => TyClDecl RdrName -> m DGG.DataType
toDGG ty  | isDataDecl ty  = toDGGData $ GHCDataDecl ty
          | isSynDecl ty   = toDGGSyn $ GHCTySynDecl ty
          | otherwise      = fail "Things other than datatypes and synonyms are not supported"

toDGGData :: Monad m => GHCDataDecl -> m DGG.DataType
toDGGData (GHCDataDecl ty) = do
  unless (null (unLoc $ tcdCtxt ty)) $
    fail "Contexts are not supported"
  when (isJust $ tcdTyPats ty) $
    fail "Type patterns are not supported"
  tvs  <- mapM (mkTyVar . unLoc)  (tcdTyVars ty)
  cts  <- mapM (mkCtor . unLoc)   (tcdCons ty)
  return DGG.DataType
    {  DGG.tyConName  = undefined -- showRdrName (unLoc (tcdLName ty))
    ,  DGG.tyKind     = DGG.KindStar -- TODO
    ,  DGG.tyVars     = tvs
    ,  DGG.ctors      = cts
    }

mkTyVar :: (Outputable a, Monad m) => HsTyVarBndr a -> m String
mkTyVar (UserTyVar n ptck)  = return $ showSDoc ( ppr n )
mkTyVar _                   = fail "Explicitly kinded type variables are not supported yet"

mkCtor :: Monad m => ConDecl name -> m DGG.Con
mkCtor cdcl = return DGG.Con
  {  DGG.conName    = undefined
  ,  DGG.conFixity  = undefined
  ,  DGG.conFields  = undefined
  }

toDGGSyn :: Monad m => GHCTySynDecl -> m DGG.DataType
toDGGSyn (GHCTySynDecl syn) = undefined

{- kind_ :: Bool -> String -> IO (Type, Kind)-}
{- kind_ b t =-}
  {- defaultErrorHandler defaultLogAction $ do-}
    {- runGhc (Just libdir) $ do-}
      {- dflags <- getSessionDynFlags-}
      {- setSessionDynFlags dflags-}
      {- load LoadAllTargets-}
      {- setContext [(IIDecl (simpleImportDecl (mkModuleName "Prelude")))]-}
      {- typeKind b t-}

{- type_ :: String -> IO Type-}
{- type_ t =-}
  {- defaultErrorHandler defaultLogAction $ do-}
    {- runGhc (Just libdir) $ do-}
      {- dflags <- getSessionDynFlags-}
      {- setSessionDynFlags dflags-}
      {- load LoadAllTargets-}
      {- setContext [(IIDecl (simpleImportDecl (mkModuleName "Prelude")))]-}
      {- exprType t-}

