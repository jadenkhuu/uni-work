use std::{
    error::Error,
    fs::File,
    io::{Read, stdin},
    path::{Path, PathBuf},
};

use itertools::Itertools;
// use std::collections::HashMap;

use clap::Parser;
use ortalib::{Chips, Mult, Round, PokerHand};
use ortalib::{Card, Suit, Rank};

#[derive(Parser)]
struct Opts {
    file: PathBuf,

    #[arg(long)]
    explain: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let round = parse_round(&opts)?;

    // Debug to print sorted lists by rank and suit
    // println!("{:?}", sorted_by_rank(&round));
    // println!("{:?}", sorted_by_suit(&round));

    let (chips, mult) = score(round);

    println!("{}", (chips * mult).floor());
    Ok(())
}

fn parse_round(opts: &Opts) -> Result<Round, Box<dyn Error>> {
    let mut input = String::new();
    if opts.file == Path::new("-") {
        stdin().read_to_string(&mut input)?;
    } else {
        File::open(&opts.file)?.read_to_string(&mut input)?;
    }

    let round = serde_yaml::from_str(&input)?;
    Ok(round)
}

// pub enum PokerHand {
//     HighCard = 0,
//     Pair = 1,
//     TwoPair = 2,
//     ThreeOfAKind = 3,
//     Straight = 4,
//     Flush = 5,
//     FullHouse = 6,
//     FourOfAKind = 7,
//     StraightFlush = 8,
//     FiveOfAKind = 9,
//     FlushHouse = 10,
//     FlushFive = 11,
// }
fn score(round: Round) -> (Chips, Mult) {
    // let poker_hand_made = make_hand(&round);
    make_hand(&round)

    // debug print hand made
    // println!("{:?}", poker_hand_made);

//     let (c, m) = match &poker_hand_made{
//         PokerHand::StraightFlush => PokerHand::StraightFlush.hand_value(),
//         PokerHand::FourOfAKind => PokerHand::FourOfAKind.hand_value(),
//         PokerHand::Flush => PokerHand::Flush.hand_value(),
//         PokerHand::Straight => PokerHand::Straight.hand_value(),
//         PokerHand::ThreeOfAKind => calc_triple(&round),
//         PokerHand::TwoPair => PokerHand::TwoPair.hand_value(),
//         PokerHand::Pair => calc_pair(&round),
//         _ => calc_highcard(&round),
//     };
//     (c, m)
}

// Takes the cards_played and finds what poker hand it is
// Returns poker hand as PokerHand
fn make_hand(round: &Round) -> (f64, f64) {
    // if check_triple(&sorted_by_rank(round)) {
    //     calc_triple(&round)
    // } else
    if check_pair(&sorted_by_rank(round)) {
        // calc_pair(&round)
        (1.0, 1.0)
    } else {
        calc_highcard(&round)
    }
}

// HIGH CARD //
// Calculates (Chips, Mult) of a High Card hand
fn calc_highcard(round: &Round) -> (f64, f64) {
    let (c, m) = PokerHand::HighCard.hand_value();
    (c + sorted_by_rank(round)[0].rank.rank_value(), m)
}

// PAIR //
// Checks for Pair | Calculate (Chips, Mult) of a pair
fn check_pair(cards: &[Card]) -> bool {
    cards.iter()
        .chunk_by(|card| card.rank) // Group by rank
        .into_iter()
        .any(|(_, group)| group.count() == 2)
}
fn calc_pair(round: &Round) -> (f64, f64) {
    // hand value mult
    let (c, m) = PokerHand::Pair.hand_value();
    let cards = &sorted_by_rank(round);

    todo!()
}

// TWO PAIR //
// fn check_twopair(cards: &[Card]) -> bool {
//     let pair_count = cards.iter()
//     .map(|card| card.rank)
//     .sorted()
//     .chunk_by(|&rank| rank)
//     .into_iter()
//     .filter(|(_, group)| group.clone().count() == 2)
//     .count();

// pair_count == 2
// // returns true if 2 pairs exist
// }
// fn calc_twopair(round: &Round) -> (f64, f64) {

//     todo!()
// }

// THREE OF A KIND //
// Checks for Three of a kind | Calculates (Chips, Mult) of Triple
// fn check_triple(cards: &[Card]) -> bool {
//     cards.iter()
//     .map(|card| card.rank)
//     .sorted()
//     .chunk_by(|&rank| rank)
//     .into_iter()
//     .any(|(_, group)| group.count() == 3)
// }
// fn calc_triple(round: &Round) -> (f64, f64) {
//     todo!()
// }

// Sorts in descending order by rank
fn sorted_by_rank(round: &Round) -> Vec<Card> {
    round.cards_played.iter()
        .sorted_by_key(|card| std::cmp::Reverse(card.rank))
        .cloned() // convert back to Vec<Card>
        .collect()
}

// Sorts and groups into suits (spade, heart, club, diamond) while keeping rank order
fn sorted_by_suit(round: &Round) -> Vec<Card> {
    round.cards_played.iter()
        .sorted_by_key(|card| (card.suit, card.rank))
        .cloned()
        .collect()
}
