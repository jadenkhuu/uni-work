fn main() {
    // Rust doesnt allow multiple mutable references to the same data, vec
    let mut vec = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    {
        let a = &mut vec;
        a.push(11);
    }
    // this allows 'a' to be used and then once it goes out of scope, 'b' can referenced

    let b = &mut vec;
    b.push(12);
}
