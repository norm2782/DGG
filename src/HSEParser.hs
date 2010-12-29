module Main where

import Language.Haskell.Exts
import DGG.Data

--testFile = "testmodule.hs"
--testFile = "EMGMTestData.hs"
testFile = "Reps.hs"

main =
    do pr <- parseFile testFile
       do case pr of
            (ParseOk a)       -> processModule a 
            (ParseFailed _ m) -> putStrLn m

processModule :: Module -> IO ()
processModule (Module sl mn mps mwt mess ids ds) =
    do  printSrcLoc sl
        printModuleInfo mn
        printPragmas mps
        printWarning mwt
        printExports mess
        printImports ids
        printDecls ds

printSrcLoc :: SrcLoc -> IO ()
printSrcLoc (SrcLoc fn l c) = putStrLn $ fn ++ " l: " ++ show l ++ " c: " ++ show c

printModuleInfo :: ModuleName -> IO ()
printModuleInfo (ModuleName mn) = putStrLn mn

printWarning :: Maybe WarningText -> IO ()
printWarning Nothing = putStrLn "(No warnings)"
printWarning (Just (DeprText dt)) = putStrLn $ "Depr: " ++ dt
printWarning (Just (WarnText wt)) = putStrLn $ "Warn: " ++ wt

printPragmas mps  = putStrLn "(Pragmas aren't that interesting for me)"
printExports mess = putStrLn "(A module's exports are not that interesting for me either)"
printImports ids  = putStrLn "(A module's imports are not that interesting for me either)"
printDecls   ds   = sequence_ $ map (\x -> putStrLn (show x ++ "\n")) ds --putStrLn "The Decls is what it's all about!"

-- Looking at data Decl in Language.Haskell.Exts.Syntax:
-- We're probably interested in the following information, of which a couple of
-- are probably not that interesting after all:
{-
-- A type declaration
TypeDecl SrcLoc Name [TyVarBind] Type

-- A type family declaration
TypeFamDecl SrcLoc Name [TyVarBind] (Maybe Kind)

-- A data OR newtype declaration
DataDecl SrcLoc DataOrNew Context Name [TyVarBind] [QualConDecl] [Deriving]

--A data OR newtype declaration, GADT style
GDataDecl SrcLoc DataOrNew Context Name [TyVarBind] (Maybe Kind) [GadtDecl] [Deriving]

-- A data family declaration
DataFamDecl SrcLoc Context Name [TyVarBind] (Maybe Kind)

-- A type family instance declaration
TypeInsDecl SrcLoc Type Type

-- A data family instance declaration
DataInsDecl SrcLoc DataOrNew Type [QualConDecl] [Deriving]

-- A data family instance declaration, GADT style
GDataInsDecl SrcLoc DataOrNew Type (Maybe Kind) [GadtDecl] [Deriving]

-- A declaration of a type class
ClassDecl SrcLoc Context Name [TyVarBind] [FunDep] [ClassDecl]

-- An declaration of a type class instance
InstDecl SrcLoc Context QName [Type] [InstDecl]	

-- A standalone deriving declaration
DerivDecl SrcLoc Context QName [Type]

-- A declaration of operator fixity
InfixDecl SrcLoc Assoc Int [Op]

-- A declaration of default types
DefaultDecl SrcLoc [Type]

-- A Template Haskell splicing declaration
SpliceDecl SrcLoc Exp

-- A type signature declaration
TypeSig SrcLoc [Name] Type

-- A set of function binding clauses
FunBind [Match]

-- A pattern binding
PatBind SrcLoc Pat (Maybe Type) Rhs Binds
-}

-- Where to go from here?
-- Two things are required by this library:
-- 1. It needs to be able to parse datatypes and convert them into a simpler
--    format so the second point can be achieved.
-- 2. It needs to be able to generate representations for various generic views
--    and libraries.
-- 3. Think of a cool name for this library
--
-- In order to achieve 1:
-- - Determine what information is required from the datatypes.
-- - Define a simpler format to contain this information
-- - Write conversion functions from haskell-src-exts to the library representation
--
-- In order to achieve 2:
-- - Map the library representation back to haskell-src-exts
-- - Hook it up to mechanisms as found in Derive, making sure there is an option
--   for both outputting to file (compiler independent) and using TH (GHC only)
--
-- In order to achieve 3:
-- - Be creative

-- Required data (TODO: Make a separate document for this):
-- 
-- - Constructor: at the very least its name, arity, labels (for record syntax)
--   and its fixity. These are required by EMGM, which is probably the most 
--   demanding for constructor data. Also observe the type of constructor:
--   normal prefix, record style (prefix or infix) and infix without records.
