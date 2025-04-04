use std::io::{self, Write};

fn main() {
    print!("What is your name? ");
    io::stdout().flush().unwrap();

    let mut name = String::new();
    match io::stdin().read_line(&mut name) {
        Ok(_n) => {
            if name.trim().is_empty() {
                println!("No name entered :(, goodbye.");
            } else {
                println!("Hello, {}, nice to meet you!", name.trim());
            }
        }
        Err(error) => println!("error: {error}"),
    }
}
