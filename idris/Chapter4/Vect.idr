import Data.Fin

data Vect : Nat -> Type -> Type where
    Nil : Vect Z a
    (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : Vect n a -> Vect n b -> Vect n (a,b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

vectTake : (n: Nat) -> Vect (n + m) a -> Vect n a
vectTake Z _ = []
vectTake (S k) (x :: xs) = x :: vectTake k xs
