%include polycode.fmt
\begin{code}
list = DataDecl (srcLoc 1) DataType [] (Ident "List") [UnkindedVar (Ident "a")]
    [ QualConDecl (srcLoc 15) [] [] (ConDecl (Ident "Nil") [])
    , QualConDecl (srcLoc 21) [] [] (ConDecl (Ident "Cons")
        [ UnBangedTy (TyVar (Ident "a"))
        , UnBangedTy (TyParen (TyApp (TyCon (UnQual (Ident "List"))) (TyVar (Ident "a"))))])
    ] []
    where srcLoc n = SrcLoc "List.hs" 1 n
\end{code}
