module Shape

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
