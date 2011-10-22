{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad
import System.Environment
import Language.Haskell.Exts

main :: IO ()
main =
    do args <- getArgs
       when (null args) $ error "Filename required"
       pr <- parseFile $ head args
       case pr of
         (ParseOk a)       -> processModule a
         (ParseFailed _ m) -> putStrLn m

processModule :: Module -> IO ()
processModule (Module _ _ _ _ _ _ ds) = mapM_ (\x -> putStrLn (show x ++ "\n")) ds

