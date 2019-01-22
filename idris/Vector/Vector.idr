module Vector

import Data.IOArray

data Vector elem = MkVector Int (IO (IOArray elem))

newVector : Int -> Vector elem
newVector {elem} len = MkVector len $ newArray len (the elem $ believe_me Void)

empty : Vector a
empty = newVector 0

null : Vector a -> Bool
null (MkVector len _) = len == 0

unsafeReadVector : Vector elem -> Int -> elem
unsafeReadVector (MkVector len ioIOArray) idx = unsafePerformIO $ do
  ioArray <- ioIOArray
  unsafeReadArray ioArray idx

unsafeWriteVector : Vector elem -> Int -> elem -> Vector elem
unsafeWriteVector {elem} (MkVector len ioIOArray) idx elm = MkVector len $ do
  ioArray <- ioIOArray
  let (MkVector _ ioNewIOArray) = newVector len {elem}
  newIOArray <- ioNewIOArray
  let indices = [0..len-1]
  flip traverse_ indices $ \index => if index == idx
    then
      unsafeWriteArray newIOArray index elm
    else do
      e <- unsafeReadArray ioArray index
      unsafeWriteArray newIOArray index e
  pure newIOArray

map : (a -> b) -> Vector a -> Vector b
map {b} f (MkVector len ioIOArray) = MkVector len $ do
  ioArray <- ioIOArray
  let (MkVector _ ioNewIOArray) = newVector {elem=b} len
  newIOArray <- ioNewIOArray
  let indices = [0..len-1]
  flip traverse_ indices $ \index => do
    e <- unsafeReadArray ioArray index
    unsafeWriteArray newIOArray index (f e)
  pure newIOArray

fromList : List a -> Vector a
fromList [] = newVector {elem=a} 0
fromList xs = let
  len = toIntNat $ length xs
  (MkVector _ ioNewIOArray) = newVector len {elem=a}
  indices = [0..len-1]
  in MkVector len $ do
    newIOArray <- ioNewIOArray
    flip traverse_ (zip indices xs) $ uncurry (unsafeWriteArray newIOArray)
    pure newIOArray

take : Int -> Vector a -> Vector a
take 0 _ = empty
take n input@(MkVector len ioIOArray) = if n >= len
  then input
  else MkVector n $ do
    ioArray <- ioIOArray
    let (MkVector _ ioNewIOArray) = newVector n {elem=a}
    let indices = [0..n-1]
    newIOArray <- ioNewIOArray
    flip traverse_ indices $ \index => do
      e <- unsafeReadArray ioArray index
      unsafeWriteArray newIOArray index e
    pure newIOArray

drop : Int -> Vector a -> Vector a
drop {a} 0 input = input
drop {a} n input@(MkVector len ioIOArray) = if len <= n
  then empty
  else MkVector (len-n) $ do
    ioArray <- ioIOArray
    let (MkVector _ ioNewIOArray) = newVector (len-n) {elem=a}
    let indices : List Int = [n .. (len-1)]
    newIOArray <- ioNewIOArray
    flip traverse_ indices $ \index => do
      e <- unsafeReadArray ioArray index
      unsafeWriteArray newIOArray (index-n) e
    pure newIOArray

append : Vector a -> Vector a -> Vector a
append {a} vectorA@(MkVector lenA ioIOArrayA) vectorB@(MkVector lenB ioIOArrayB) =
  if null vectorA
    then vectorB
    else if null vectorB
      then vectorA
      else MkVector (lenA + lenB) $ do
        ioArrayA <- ioIOArrayA
        ioArrayB <- ioIOArrayB
        let (MkVector _ ioNewIOArray) = newVector (lenA + lenB) {elem=a}
        newIOArray <- ioNewIOArray
        flip traverse_ [0..(lenA-1)] $ \index => do
          e <- unsafeReadArray ioArrayA index
          unsafeWriteArray newIOArray index e
        flip traverse_ [0..(lenB-1)] $ \index => do
          e <- unsafeReadArray ioArrayB index
          unsafeWriteArray newIOArray (index+lenA) e
        pure newIOArray

foldrHelper : Int -> (elem -> acc -> acc) -> acc -> Vector elem -> acc
foldrHelper index f init input@(MkVector len ioIOArray) = if index == len
  then init
  else f (unsafeReadVector input index) $ foldrHelper (index+1) f init input

Foldable Vector where
  foldr = assert_total $ foldrHelper 0

Functor Vector where
  map = Vector.map

Semigroup (Vector a) where
  (<+>) = append

Monoid (Vector a) where
  neutral = empty

main : IO ()
main = do
  let x = fromList [1..7]
  let y = fromList [8..12]
  let z = fromList [13..20]
  putStrLn $ show $ toList $ concat [x, y, z]
