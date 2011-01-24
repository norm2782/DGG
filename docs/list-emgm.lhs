%include polycode.fmt
\begin{code}
data List a = Nil | Cons a (List a)
type ListRep a = Unit :+: a :*: List a

listEP :: EP (List a) (ListRep a)
listEP = EP from to
    where
        from Nil           = L Unit
        from (Cons x xs)   = R (x :*: xs)
        to (L Unit)        = Nil
        to (R (x :*: xs))  = Cons x xs
\end{code}
