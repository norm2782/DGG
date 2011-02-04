{-# LANGUAGE TypeOperators #-}

module Main where

import System (getArgs)
import Language.Haskell.Exts

main :: IO ()
main =
    do args <- getArgs
       if length args == 0
           then error "Filename required"
           else return ()
       pr <- parseFile $ head args
       do case pr of
            (ParseOk a)       -> processModule a 
            (ParseFailed _ m) -> putStrLn m

processModule :: Module -> IO ()
processModule (Module _ _ _ _ _ _ ds) = sequence_ $ map (\x -> putStrLn (show x ++ "\n")) ds 

