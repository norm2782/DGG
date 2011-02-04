%include polycode.fmt
\begin{code}
listEP = EP from to
    where
        from Nil           = L Unit
        from (Cons x xs)   = R (x :*: xs)
        to (L Unit)        = Nil
        to (R (x :*: xs))  = Cons x xs
\end{code}
