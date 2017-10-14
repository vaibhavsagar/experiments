import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix r c = Vect r (Vect c Double)

testMatrix : Matrix 2 3
testMatrix = [[0,0,0], [0,0,0]]

TupleVect : (n : Nat) -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, TupleVect k x)

test : TupleVect 4 Nat
test = (1,2,3,4,())
