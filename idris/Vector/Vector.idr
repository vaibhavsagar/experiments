import Data.IOArray

data Vector elem = MkVector Int (IO (IOArray elem))

newVector : Int -> Vector elem
newVector {elem} len = MkVector len $ newArray len (the elem $ believe_me Void)

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
