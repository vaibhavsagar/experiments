data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)

Foldable (Vect n) where
  foldr func init [] = init
  foldr func init (x :: xs) = foldr func (func x init) xs
