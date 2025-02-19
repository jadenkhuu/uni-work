use bmp::{Image, Pixel};

fn main() {
    let path = std::env::args().nth(1).expect("You must provide a path.");
    let operation = std::env::args().nth(2).expect("You must provide an operation.");

    if operation.as_str() == "pixel" {
        draw_pixel(path.as_str());
    } else if operation.as_str() == "diagonal" {
        draw_diagonal(path.as_str());
    } else if operation.as_str() == "square" {
        draw_square(path.as_str());
    } else if operation.as_str() == "x" {
        draw_x(path.as_str());
    } else {
        eprintln!("The operation {operation} was not recognised!");
    }
}

// fn draw_diamond(path: &str) {
    
// }

fn draw_x(path: &str) {
    let mut image = Image::new(100,100);
    let width = 100;
    let height = 100;

    for row in 0..width {
        for col in 0..height {
            if row == col || row + col == 99 {
                image.set_pixel(row, col, Pixel::new(255, 255, 255));
            }
        }
    }
    image.save(path).expect("This should save correctly.");
}

fn draw_diagonal(path: &str) {
    let mut image = Image::new(100,100);
    let width = 100;
    let height = 100;

    for row in 0..width {
        for col in 0..height {
            if row == col {
                image.set_pixel(row, col, Pixel::new(255, 255, 255));
            }
        }
    }
    image.save(path).expect("This should save correctly.");
}

fn draw_square(path: &str) {
    let mut image = Image::new(100, 100);
    let square_size = 60;
    let start_x = 20; 
    let start_y = 20; 

    for x in start_x..start_x + square_size + 1 {
        for y in start_y..start_y + square_size + 1 {
            if x == 20 || x == 80 {
                image.set_pixel(x, y, Pixel::new(255, 255, 255));
            }
            if y == 20 || y == 80 {
                image.set_pixel(x, y, Pixel::new(255, 255, 255));
            }
        }
    }
    image.save(path).expect("This should save correctly.");
}

fn draw_pixel(path: &str) {
    let mut image = Image::new(100, 100);
    image.set_pixel(50, 50, Pixel::new(255, 255, 255));
    image.save(path).expect("This should save correctly.");
}
