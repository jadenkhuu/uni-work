use serde::Deserialize;
use std::collections::VecDeque;
use std::{default, io};

#[derive(Debug, Deserialize)]
enum Instruction {
    Set(i32),
    Left,
    Right,
    Reset,
}

#[derive(Debug, Default)]
struct Light {
    left: Option<Box<Light>>,
    right: Option<Box<Light>>,
    brightness: i32,
}

impl Light {
    fn left_move(&mut self) -> &mut Light {
        self.left.get_or_insert_with(|| Box::new(default::Default::default()))
    }
    fn right_move(&mut self) -> &mut Light {
        self.right.get_or_insert_with(|| Box::new(default::Default::default()))
    }
    fn total_brightness(&self) -> i32 {
        let left = self.left.as_ref().map_or(0, |l| l.total_brightness());
        let right = self.right.as_ref().map_or(0, |r| r.total_brightness());
        left + right + self.brightness
    }
    fn total_lights(&self) -> i32 {
        let left = self.left.as_ref().map_or(0, |l| l.total_lights());
        let right = self.right.as_ref().map_or(0, |r| r.total_lights());
        left + right + 1
    }
}

fn get_instructions_from_stdin() -> VecDeque<Instruction> {
    let mut instructions = String::new();
    io::stdin().read_line(&mut instructions).unwrap();
    ron::from_str(&instructions).unwrap()
}
fn main() {
    let instructions = get_instructions_from_stdin();
    let mut light = Light { left: None, right: None, brightness: 0};
    let mut curr = &mut light;
    for instruction in instructions {
        match instruction {
            Instruction::Set(x) => {
                curr.brightness = x;
            }
            Instruction::Left => {
                curr = curr.left_move()
            }
            Instruction::Right => {
                curr = curr.right_move()
            }
            Instruction::Reset => {
                curr = &mut light;
            }
        }
    }

    let total_brightness = light.total_brightness();
    let num_lights = light.total_lights();
    let avg = total_brightness / num_lights;
    println!("{avg}");

}

// use serde::Deserialize;
// use std::collections::VecDeque;
// use std::io;

// #[derive(Debug, Deserialize)]
// enum Instruction {
//     Set(i32),
//     Left,
//     Right,
//     Reset,
// }

// #[derive(Debug)]
// struct Light {
//     left: Option<Box<Light>>,
//     right: Option<Box<Light>>,
//     brightness: i32,
// }

// fn get_instructions_from_stdin() -> VecDeque<Instruction> {
//     let mut instructions = String::new();
//     io::stdin().read_line(&mut instructions).unwrap();
//     ron::from_str(&instructions).unwrap()
// }

// fn main() {
//     let instructions = get_instructions_from_stdin();
//     let light = Light { left: (), right: (), brightness: 0};
//     println!("{instructions:?}");
//     println!("{light:?}");

//     for inst in &instructions {
//         println!("{inst:?}");

//         match
//         if inst == set {

//         } else if inst == left {

//         } else if inst == right {}


//         }

//         light.brightness
//     }
// }
