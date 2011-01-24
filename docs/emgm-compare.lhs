%include polycode.fmt
\begin{code}
newtype Compare a = Compare { selCompare :: a -> a -> Ordering }

instance Generic Compare where
  rconstant      = Compare rconstantCompare
  rsum     ra rb = Compare (rsumCompare ra rb)
  rprod    ra rb = Compare (rprodCompare ra rb)
  rcon  cd ra    = Compare (rconCompare cd ra)
  rtype ep ra    = Compare (rtypeCompare ep ra)

rconstantCompare :: (Ord a) => a -> a -> Ordering
rsumCompare :: Compare a -> Compare b -> a :+: b -> a :+: b -> Ordering
rprodCompare :: Compare a -> Compare b -> a :*: b -> a :*: b -> Ordering
rprodCompare ra rb (a1 :*: b1) (a2 :*: b2) =
rconCompare :: ConDescr -> Compare a -> a -> a -> Ordering
rtypeCompare :: EP a b -> Compare b -> a -> a -> Ordering
compare :: (Rep Compare a) => a -> a -> Ordering
\end{code}
