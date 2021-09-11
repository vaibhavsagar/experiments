fn main() {
    println!("Hello, world!");

    let x = 5;

    let y = {
        let x = 3;
        x + 1
    };

    another_function(x, y);
}

fn another_function(x : i32, y: i32) {
    println!("The value of x is: {}", x);
    println!("The value of y is: {}", y);
}
