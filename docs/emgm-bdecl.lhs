%include polycode.fmt
\begin{code}
bdeclFrom :: Int -> DCInfo -> Match
bdeclFrom cnt dci = mkMatch "from"
    [pApp n (map mkPIdent $ genNames a)] (fromEP 0 cnt dci)
    where  n = dcName dci
           a = dcArity dci

bdeclTo :: Int -> DCInfo -> Match
bdeclTo cnt dci = mkMatch "to" [toEP 0 cnt dci] (mkToRhs dci)
\end{code}
