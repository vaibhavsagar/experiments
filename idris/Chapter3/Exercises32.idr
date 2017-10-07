module Exercises32

import Data.Vect

total my_length : List a -> Nat
my_length [] = 0
my_length (x :: xs) = 1 + my_length xs

total my_reverse : List a -> List a
my_reverse [] = []
my_reverse (x :: xs) = my_reverse xs ++ [x]

total my_map_list : (a -> b) -> List a -> List b
my_map_list f [] = []
my_map_list f (x :: xs) = f x :: my_map_list f xs

total my_map_vect : (a -> b) -> Vect n a -> Vect n b
my_map_vect f [] = []
my_map_vect f (x :: xs) = f x :: my_map_vect f xs
