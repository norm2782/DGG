%include polycode.fmt
\begin{code}
fromEP :: Int -> Int -> DCInfo -> Exp
fromEP = ep mkFromRs mkExpSum owFrom

owFrom :: Int -> Int -> DCInfo -> Exp
owFrom cnt nc dci = App (mkStrCon "R") (fromEP (cnt + 1) nc dci)

mkExpSum :: String -> Int -> Exp
mkExpSum s n = App (mkStrCon s) (mkFromRs n)

mkFromRs :: Int -> Exp
mkFromRs 0 = mkStrCon "Unit"
mkFromRs n = foldlInApp (QConOp . unQualSym $ ":*:")
    mkIdent $ genNames n
\end{code}
