use bmp::{Image, Pixel};

fn main() {
    let path = std::env::args().nth(1).expect("You must provide a path.");
    let operation = std::env::args().nth(2).expect("You must provide an operation.");

    if operation.as_str() == "pixel" {
        draw_pixel(path.as_str());
    } else if operation.as_str() == "something" {
        draw_something(path.as_str());
    } else {
        eprintln!("The operation {operation} was not recognised!");
    }
}

fn draw_pixel(path: &str) {
    let mut image = Image::new(100, 100);
    image.set_pixel(50, 50, Pixel::new(255, 255, 255));
    image.save(path).expect("This should save correctly.");
}

fn draw_something(path: &str) {
    let mut image = Image::new(100, 100);

    for row in 20..=80 {
        for col in 35..=80 {
            if (row <= 35 && col <= 50) || ((row >= 65 && row <= 80) && col <= 50) {
                image.set_pixel(row, col, Pixel::new(255,255,255));
            } else if row >= 20 && (col >= 70 && col <= 80) {
                image.set_pixel(row, col, Pixel::new(255,255,255));
            }
        }
    }

    image.save(path).expect("This should save correctly");
}