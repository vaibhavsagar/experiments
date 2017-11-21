module Exercises71

public export
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

%name Shape shape, shape1, shape2

export
area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

Eq Shape where
  (==) (Triangle b h) (Triangle b' h') = b == b' && h == h'
  (==) (Rectangle l h) (Rectangle l' h') = l == l' && h == h'
  (==) (Circle r) (Circle r') = r == r'
  (==) _ _ = False

Ord Shape where
  compare x y = compare (area x) (area y)
