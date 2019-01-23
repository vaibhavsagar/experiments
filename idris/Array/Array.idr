module Array

import Data.IOArray

data Array elem = MkArray Int (IO (IOArray elem))

newArray : Int -> Array elem
newArray {elem} len = MkArray len $ newArray len (the elem $ believe_me Void)

empty : Array a
empty = newArray 0

singleton : a -> Array a
singleton elem = MkArray 1 $ newArray 1 elem

null : Array a -> Bool
null (MkArray len _) = len == 0

unsafeReadArray : Array elem -> Int -> elem
unsafeReadArray (MkArray len ioIOArray) idx = unsafePerformIO $ do
  ioArray <- ioIOArray
  unsafeReadArray ioArray idx

unsafeWriteArray : Array elem -> Int -> elem -> Array elem
unsafeWriteArray {elem} (MkArray len ioIOArray) idx elm = MkArray len $ do
  ioArray <- ioIOArray
  let (MkArray _ ioNewIOArray) = newArray len {elem}
  newIOArray <- ioNewIOArray
  let indices = [0..len-1]
  flip traverse_ indices $ \index => if index == idx
    then
      unsafeWriteArray newIOArray index elm
    else do
      e <- unsafeReadArray ioArray index
      unsafeWriteArray newIOArray index e
  pure newIOArray

map : (a -> b) -> Array a -> Array b
map {b} f (MkArray len ioIOArray) = MkArray len $ do
  ioArray <- ioIOArray
  let (MkArray _ ioNewIOArray) = newArray {elem=b} len
  newIOArray <- ioNewIOArray
  let indices = [0..len-1]
  flip traverse_ indices $ \index => do
    e <- unsafeReadArray ioArray index
    unsafeWriteArray newIOArray index (f e)
  pure newIOArray

fromList : List a -> Array a
fromList [] = newArray {elem=a} 0
fromList xs = let
  len = toIntNat $ length xs
  (MkArray _ ioNewIOArray) = newArray len {elem=a}
  indices = [0..len-1]
  in MkArray len $ do
    newIOArray <- ioNewIOArray
    flip traverse_ (zip indices xs) $ uncurry (unsafeWriteArray newIOArray)
    pure newIOArray

take : Int -> Array a -> Array a
take 0 _ = empty
take n input@(MkArray len ioIOArray) = if n >= len
  then input
  else MkArray n $ do
    ioArray <- ioIOArray
    let (MkArray _ ioNewIOArray) = newArray n {elem=a}
    let indices = [0..n-1]
    newIOArray <- ioNewIOArray
    flip traverse_ indices $ \index => do
      e <- unsafeReadArray ioArray index
      unsafeWriteArray newIOArray index e
    pure newIOArray

drop : Int -> Array a -> Array a
drop {a} 0 input = input
drop {a} n input@(MkArray len ioIOArray) = if len <= n
  then empty
  else MkArray (len-n) $ do
    ioArray <- ioIOArray
    let (MkArray _ ioNewIOArray) = newArray (len-n) {elem=a}
    let indices : List Int = [n .. (len-1)]
    newIOArray <- ioNewIOArray
    flip traverse_ indices $ \index => do
      e <- unsafeReadArray ioArray index
      unsafeWriteArray newIOArray (index-n) e
    pure newIOArray

append : Array a -> Array a -> Array a
append {a} vectorA@(MkArray lenA ioIOArrayA) vectorB@(MkArray lenB ioIOArrayB) =
  if null vectorA
    then vectorB
    else if null vectorB
      then vectorA
      else MkArray (lenA + lenB) $ do
        ioArrayA <- ioIOArrayA
        ioArrayB <- ioIOArrayB
        let (MkArray _ ioNewIOArray) = newArray (lenA + lenB) {elem=a}
        newIOArray <- ioNewIOArray
        flip traverse_ [0..(lenA-1)] $ \index => do
          e <- unsafeReadArray ioArrayA index
          unsafeWriteArray newIOArray index e
        flip traverse_ [0..(lenB-1)] $ \index => do
          e <- unsafeReadArray ioArrayB index
          unsafeWriteArray newIOArray (index+lenA) e
        pure newIOArray

foldrHelper : Int -> (elem -> acc -> acc) -> acc -> Array elem -> acc
foldrHelper index f init input@(MkArray len ioIOArray) = if index == len
  then init
  else f (unsafeReadArray input index) $ foldrHelper (index+1) f init input

Foldable Array where
  foldr = assert_total $ foldrHelper 0

Functor Array where
  map = Array.map

Semigroup (Array a) where
  (<+>) = append

Monoid (Array a) where
  neutral = empty

main : IO ()
main = do
  let x = fromList [1..7]
  let y = fromList [8..12]
  let z = fromList [13..20]
  putStrLn $ show $ toList $ concat [x, y, z]
