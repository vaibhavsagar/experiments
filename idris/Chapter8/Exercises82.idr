import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in plusSuccRightSucc m k

reverseProof_nil : (acc : Vect n a) -> Vect (plus n 0) a
reverseProof_nil {n} acc = rewrite plusZeroRightNeutral n in acc

reverseProof_xs : Vect ((S m) + n) a -> Vect (m + (S n)) a
reverseProof_xs {m} {n} xs = rewrite sym (plusSuccRightSucc m n) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)
