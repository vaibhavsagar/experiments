data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
    Bicycle : Vehicle Pedal
    Unicycle : Vehicle Pedal
    Motorcycle : (fuel : Nat) -> Vehicle Petrol
    Car : (fuel : Nat) -> Vehicle Petrol
    Bus : (fuel : Nat) -> Vehicle Petrol
    Tram : (capacity : Nat) -> Vehicle Electric
    ElectricCar : (capacity : Nat) -> Vehicle Electric

wheel : Vehicle power -> Nat
wheel Bicycle = 2
wheel (Car fuel) = 4
wheel (Bus fuel) = 4
wheel Unicycle = 1
wheel (Motorcycle fuel) = 2
wheel (Tram capacity) = 6
wheel (ElectricCar capacity) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 50
