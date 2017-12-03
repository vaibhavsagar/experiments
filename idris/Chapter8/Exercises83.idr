data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect k a -> Vect (S k) a

headUnequal : DecEq a => (xs : Vect n a) -> (ys : Vect n a) ->
       (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
headUnequal ys ys contra Refl = contra Refl

tailUnequal : DecEq a => (xs : Vect n a) -> (ys : Vect n a) ->
       (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
tailUnequal ys ys contra Refl = contra Refl

DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: y) (z :: w) = case decEq x z of
                                 (Yes Refl) => (case decEq y w of
                                                     (Yes Refl) => Yes Refl
                                                     (No contra) => No (tailUnequal y w contra))
                                 (No contra) => No (headUnequal y w contra)
