data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

notInNil : Elem value [] -> Void
notInNil Here impossible
notInNil (There _) impossible

notInTail : (notThere : Elem value xs -> Void) -> (notHere : (value = x) -> Void) -> Elem value (x :: xs) -> Void
notInTail notThere notHere Here = notHere Refl
notInTail notThere notHere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : List a) -> Dec (Elem value xs)
isElem value [] = No notInNil
isElem value (x :: xs) = case decEq value x of
                              (Yes Refl) => Yes Here
                              (No notHere) => (case isElem value xs of
                                                   (Yes prf) => Yes (There prf)
                                                   (No notThere) => No (notInTail notThere notHere))

data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

notLastInNil : Last [] value -> Void
notLastInNil LastOne impossible
notLastInNil (LastCons _) impossible

notIsLast : (notLast : (value = x) -> Void) -> Dec (Last [x] value)
notIsLast notLast = ?hole_rhs

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notLastInNil
isLast [x] value = case decEq value x of
                        (Yes Refl) => Yes LastOne
                        (No notLast) => notIsLast notLast
isLast (x :: xs) value = ?isLast_rhs_2
