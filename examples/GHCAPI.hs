module GHCAPI where

import DynFlags
import GHC
import GHC.Paths ( libdir )
import MonadUtils
import Outputable


-- DGG Imports
import qualified DGG.Types as DGG

targetFile = "examples/testmodule.hs"

main :: IO ()
main = do
   res <- example
   return ()
   {- putStrLn "parsedSource: "-}
   {- putStrLn $ showSDoc ( ppr res )-}

{- example :: IO ModSummary-}
example =
    defaultErrorHandler defaultLogAction $ do
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
        ts <- getTys ps
        {- liftIO $ putStrLn "getTys:"-}
        {- liftIO $ print ts-}
        return $ ps--,"\n-----\n",  typecheckedSource d)

{- getTys :: GhcMonad m => ParsedSource -> m [DataType]-}
getTys ps = do
  let mod = unLoc ps
  let decls = hsmodDecls mod
  let hsdecls = map unLoc decls
  let tydecls = [d | TyClD d <- hsdecls]
  let datas = [d | d@(TyData _ _ _ _ _ _ _ _) <- tydecls]
  let tysyns = [d | d@(TySynonym _ _ _ _) <- tydecls]
  {- liftIO $ putStrLn $ showSDoc ( ppr datas )-}
  {- liftIO $ putStrLn $ showSDoc ( ppr tysyns )-}
  return $ datas ++ tysyns -- $ DGG.DataType {}



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

