data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node x z w) (Node y s t)
      = x == y && z == s && w == t
  (==) _ _ = False

Functor Tree where
  map func Empty = Empty
  map func (Node left e right) =
    Node (map func left)
         (func e)
         (map func right)

Foldable Tree where
  foldr func init Empty = init
  foldr func init (Node l e r) = let
    l' = foldr func init l
    r' = foldr func l' r
    e' = func e r'
    in e'
