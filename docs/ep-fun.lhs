%include polycode.fmt
\begin{code}
ep  :: (Int -> a) -> (String -> Int -> a) -> (Int -> Int -> DCInfo -> a)
    -> Int -> Int -> DCInfo -> a
ep  mkr  _    _   _    1   dci  =  mkr $ dcArity dci
ep  _    mks  ow  cnt  nc  dci  |  i == cnt + 1 && i == nc - 1  = mks "R" a
                                |  i == cnt                     = mks "L" a
                                |  otherwise                    = ow cnt nc dci
                                where  a  = dcArity  dci
                                       i  = dcIndex  dci
\end{code}
