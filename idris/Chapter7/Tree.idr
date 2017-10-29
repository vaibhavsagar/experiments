data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node x z w) (Node y s t)
      = x == y && z == s && w == t
  (==) _ _ = False
