use std::io;

fn main() {
    let pattern_string = std::env::args()
        .nth(1)
        .expect("missing required command-line argument: <pattern>");

    let pattern = &pattern_string;

    // TODO: Replace the following with your code:
    println!("The command-line argument is: {pattern}");

    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                if input.trim().is_empty() == true {
                    break;
                } else if grep_func(&input, pattern) {
                    print!("{input}");
                }
            }
            Err(error) => println!("error: {error}"),
        }
    }
}

fn grep_func(input: &String, pattern: &String) -> bool {
    input.contains(pattern)
}