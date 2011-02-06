%include polycode.fmt
\begin{code}
list = TCInfo (Ident "List") TyDataType [TCVar (Ident "a") Nothing]
    [  DCInfo (Ident "Nil")   0 Prefix []
    ,  DCInfo (Ident "Cons")  1 Prefix
         [  DCVar Nothing (TyVar (Ident "a"))
         ,  DCVar Nothing (TyParen (TyApp  (TyCon (UnQual (Ident "List")))
                                           (TyVar (Ident "a"))))]
    ]
\end{code}
