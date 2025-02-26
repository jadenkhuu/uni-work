use std::io;

fn main() {
    println!("What is your name? ");

    let mut name = String::new();
    match io::stdin().read_line(&mut name) {
        Ok(_n) => {
            if name.trim().is_empty() {
                println!("No name entered :(, goodbye");
            } else {
                println!("Hello, {}, nice to meet you!", name);
            }
        }
        Err(error) => println!("error: {error}"),
    }
}
