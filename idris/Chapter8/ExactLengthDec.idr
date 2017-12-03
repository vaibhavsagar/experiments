import Data.Vect

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input = case decEq m len of
                                 (Yes Refl) => Just input
                                 (No contra) => Nothing
