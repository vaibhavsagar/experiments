data TakeN : List a -> Type where
  Fewer : TakeN xs
  Exact : (n_xs : List a) -> TakeN (n_xs ++ rest)

total
takeN : (n : Nat) -> (xs : List a) -> TakeN xs
takeN Z _ = Exact []
takeN (S k) [] = Fewer
takeN (S k) (x :: xs) = ?hole_3
