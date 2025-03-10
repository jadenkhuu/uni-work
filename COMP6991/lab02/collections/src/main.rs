use std::collections::VecDeque;
use std::collections::LinkedList;
use std::collections::HashMap;

const MAX_ITER: i32 = 300000;

fn main() {
    // Vectors
    vec_operations();

    // VecDeque
    vec_deque_operations();

    // TODO: your code here, for linked list insertions
    linkedlist_operations();

    // TODO: your code here, for hashmap insertions
    hashmap_operations();

    // TODO: your text explanation to the questions in the spec
    // Which collection type was the fastest for adding and removing elements?
        // The fastest seems to be Vector, when both insert and remove are summed
    // Why do you think this was the case?
        

    // Is there any significant difference between Vec and VecDeque deletion?
    // If so, why? If not, why not?

    // When would you consider using VecDeque over Vec?

    // When would you consider using LinkedList over Vec?

    // Did the results suprise you? Why or why not?.
}

// measure the insertion and removal
// operations of a hash map
fn hashmap_operations() {
    // Use with_capacity to avoid resizing overhead
    let mut map: HashMap<i32, i32> = HashMap::with_capacity(MAX_ITER as usize);

    // Insert values
    let time_start = std::time::Instant::now();
    for i in 0..MAX_ITER {
        map.insert(i, i); // Insert at key `i`
    }
    let time_end = std::time::Instant::now();

    println!("==== HashMap ====");
    println!("insert: {:?}", time_end - time_start);

    // Remove values
    let time_start = std::time::Instant::now();
    for i in 0..MAX_ITER {
        map.remove(&i); // Remove from key `i`
    }
    let time_end = std::time::Instant::now();

    println!("remove: {:?}", time_end - time_start);
}

// measure the insertion and removal
// operations of a linked list
fn linkedlist_operations() {
    let mut list: LinkedList<i32> = LinkedList::new();

    let time_start = std::time::Instant::now();
    for i in 0..MAX_ITER {
        list.push_back(i);
    }
    let time_end = std::time::Instant::now();

    println!("==== Linked List ====");
    println!("insert: {:?}", time_end - time_start);

    let time_start = std::time::Instant::now();
    for _ in 0..MAX_ITER {
        list.pop_front();
    }
    let time_end = std::time::Instant::now();

    println!("remove: {:?}", time_end - time_start);
}

/// measure the insertion and removal
/// operations of a vector
fn vec_operations() {
    let mut vec = Vec::new();

    let time_start = std::time::Instant::now();
    for i in 0..MAX_ITER {
        vec.push(i);
    }
    let time_end = std::time::Instant::now();

    println!("==== Vector ====");
    println!("insert: {:?}", time_end - time_start);

    let time_start = std::time::Instant::now();
    for _ in 0..MAX_ITER {
        vec.remove(0);
    }
    let time_end = std::time::Instant::now();

    println!("remove: {:?}", time_end - time_start);
}

/// measure the insertion and removal
/// operations of a VecDeque
fn vec_deque_operations() {
    let mut vec_deque = VecDeque::new();

    let time_start = std::time::Instant::now();
    for i in 0..MAX_ITER {
        vec_deque.push_back(i);
    }
    let time_end = std::time::Instant::now();

    println!("==== VecDeque ====");
    println!("insert: {:?}", time_end - time_start);

    let time_start = std::time::Instant::now();
    for _ in 0..MAX_ITER {
        vec_deque.pop_front();
    }
    let time_end = std::time::Instant::now();

    println!("remove: {:?}", time_end - time_start);
}
