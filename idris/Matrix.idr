module Matrix

import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties {n} = replicate n []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix = zipWith addCol
    where
      addCol : Num a => Vect m a -> Vect m a -> Vect m a
      addCol = zipWith (+)

multHelper : Num a => Vect m a -> Vect p (Vect m a) -> Vect p a
multHelper xs = map (\row => sum (zipWith (*) xs row))

multMatrix : Num a =>
             Vect n (Vect m a) -> Vect m (Vect p a) ->
             Vect n (Vect p a)
multMatrix [] _ = []
multMatrix (x :: xs) ys = let
    ysTrans = transposeMat ys
    in multHelper x ysTrans :: multMatrix xs ys
