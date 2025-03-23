// use std::{
//     error::Error,
//     fs::File,
//     io::{Read, stdin},
//     path::{Path, PathBuf},
// };

// use itertools::Itertools;
// // use std::collections::HashMap;

// use clap::Parser;
// use ortalib::{Chips, Mult, Round, Card, Suit, Rank, PokerHand};

// #[derive(Parser)]
// struct Opts {
//     file: PathBuf,

//     #[arg(long)]
//     explain: bool,
// }

// fn main() -> Result<(), Box<dyn Error>> {
//     let opts = Opts::parse();
//     let round = parse_round(&opts)?;

//     // let sorted_by_rank = sorted_by_rank(&round);
//     // let sorted_by_suit = sorted_by_suit(&round);

//     // println!("Sorted by rank: {:?}", sorted_by_rank);
//     // println!("Sorted by suit: {:?}", sorted_by_suit);

//     let (chips, mult) = score(round);

//     println!("{}", (chips * mult).floor());
//     Ok(())
// }

// fn parse_round(opts: &Opts) -> Result<Round, Box<dyn Error>> {
//     let mut input = String::new();
//     if opts.file == Path::new("-") {
//         stdin().read_to_string(&mut input)?;
//     } else {
//         File::open(&opts.file)?.read_to_string(&mut input)?;
//     }

//     let round = serde_yaml::from_str(&input)?;
//     Ok(round)
// }

// // pub enum PokerHand {
// //     HighCard = 0,
// //     Pair = 1,
// //     TwoPair = 2,
// //     ThreeOfAKind = 3,
// //     Straight = 4,
// //     Flush = 5,
// //     FullHouse = 6,
// //     FourOfAKind = 7,
// //     StraightFlush = 8,
// //     FiveOfAKind = 9,
// //     FlushHouse = 10,
// //     FlushFive = 11,
// // }
// fn score(round: Round) -> (Chips, Mult) {
//     let (c, m) = match (make_hand(&round)) {
//         PokerHand::StraightFlush => PokerHand::StraightFlush.hand_value(),
//         PokerHand::FourOfAKind => PokerHand::FourOfAKind.hand_value(),
//         PokerHand::Flush => PokerHand::Flush.hand_value(),
//         PokerHand::Straight => PokerHand::Straight.hand_value(),
//         PokerHand::ThreeOfAKind => PokerHand::ThreeOfAKind.hand_value(),
//         PokerHand::TwoPair => PokerHand::TwoPair.hand_value(),
//         PokerHand::Pair => PokerHand::Pair.hand_value(),
//         _ => PokerHand::HighCard.hand_value(),
//     }

//     todo!()
// }

// // Takes the cards_played and finds what poker hand it is
// // Returns poker hand as PokerHand
// fn make_hand(round: &Round) -> PokerHand {

//     todo!()
// }

// fn sorted_by_rank(round: &Round) -> Vec<Card> {
//     let mut sorted = round.cards_played.clone();
//     sorted.sort_by_key(|card| card.rank);
//     sorted
// }

// fn sorted_by_suit(round: &Round) -> Vec<Card> {
//     let mut sorted = round.cards_played.clone();
//     sorted.sort_by_key(|card| (card.suit, card.rank));
//     sorted
// }
