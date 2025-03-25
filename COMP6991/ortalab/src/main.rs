use std::{
    error::Error,
    fs::File,
    io::{Read, stdin},
    path::{Path, PathBuf},
};

use itertools::Itertools;
// use std::collections::HashMap;

use clap::Parser;
use ortalib::{Chips, Mult, Round, PokerHand, Card};
// use ortalib::{Suit, Rank};

#[derive(Parser)]
struct Opts {
    file: PathBuf,

    #[arg(long)]
    explain: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opts = Opts::parse();
    let round = parse_round(&opts)?;
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

fn score(round: Round) -> (Chips, Mult) {
    make_hand(&round)
}

// Takes the cards_played and finds what poker hand it is
// Calculates hand value and returns
fn make_hand(round: &Round) -> (f64, f64) {
    let cards_by_rank = &sorted_by_rank(round);
    let cards_by_suit = &sorted_by_suit(round);

    // Debugging prints
    println!("{:?}", cards_by_rank);
    println!("{:?}", cards_by_suit);

    if check_straight(&cards_by_rank) {
        println!("Straight");
        calc_straight(&cards_by_rank)
    }
    else if check_triple(&cards_by_rank) {
        println!("Three of a Kind");
        calc_triple(&cards_by_rank)
    }
    else if check_twopair(&cards_by_rank) {
        println!("Two Pair");
        calc_twopair(&cards_by_rank)
    }
    else if check_pair(&cards_by_rank) {
        println!("Pair");
        calc_pair(&cards_by_rank)
    }
    else {
        println!("High card");
        calc_highcard(&cards_by_rank)
    }

    // Order
    // Straight Flush
    // Four of a Kind
    // Full House
    // Flush
    // Straight
    // Three of a Kind x
    // Two Pair x
    // Pair x
    // High Card x

}

fn check_straight(cards: &[Card]) -> bool {

    // ace can only be at the start or end of a straight,
    todo!()
}
fn calc_straight(cards: &[Card]) -> (f64, f64) {
    let (c, m) = PokerHand::Straight.hand_value();
    (c, m)
}

// THREE OF A KIND //
// Checks for Three of a kind | Calculates (Chips, Mult) of Triple
fn check_triple(cards: &[Card]) -> bool {
    cards.windows(3)
        .any(|pair| pair[0].rank == pair[1].rank && pair[1].rank == pair[2].rank)
}
fn calc_triple(cards: &[Card]) -> (f64, f64) {
    let (c, m) = PokerHand::ThreeOfAKind.hand_value();
    let pairs = cards.windows(3)
    .filter(|pair| pair[0].rank == pair[1].rank && pair[1].rank == pair[2].rank) // Find pairs with the same rank
    .map(|pair| pair[0]) // Extract the rank of each pair
    .collect::<Vec<_>>();

    // debugging
    println!("{:?}", pairs);

    (c + pairs[0].rank.rank_value() * 3.0, m)
}

// TWO PAIR //
// Checks for Two Pair | Calculate (Chips, Mult) of a Two Pair
fn check_twopair(cards: &[Card]) -> bool {
    let pairs = cards.windows(2)
    .filter(|pair| pair[0].rank == pair[1].rank) // Find pairs with the same rank
    .map(|pair| pair[0]) // Extract the rank of each pair
    .collect::<Vec<_>>(); // Collect ranks of pairs into a vector

    println!("{:?}", pairs);
    pairs.len() == 2
}
fn calc_twopair(cards: &[Card]) -> (f64, f64) {
    let (c, m) = PokerHand::TwoPair.hand_value();
    let pairs = cards.windows(2)
    .filter(|pair| pair[0].rank == pair[1].rank) // Find pairs with the same rank
    .map(|pair| pair[0]) // Extract the rank of each pair
    .collect::<Vec<_>>();

    (c + pairs[0].rank.rank_value() + pairs[1].rank.rank_value(), m)
}

// PAIR //
// Checks for Pair | Calculate (Chips, Mult) of a pair
fn check_pair(cards: &[Card]) -> bool {
    cards.windows(2)
        .any(|pair| pair[0].rank == pair[1].rank)
}
fn calc_pair(cards: &[Card]) -> (f64, f64) {
    let (c, m) = PokerHand::Pair.hand_value();
    let paired_card = cards.windows(2)
        .find(|pair| pair[0].rank == pair[1].rank)
        .map(|pair| pair[0])
        .unwrap();

    (c + paired_card.rank.rank_value() + paired_card.rank.rank_value(), m)
}

// HIGH CARD //
// Calculates (Chips, Mult) of a High Card hand
fn calc_highcard(cards: &[Card]) -> (f64, f64) {
    let (c, m) = PokerHand::HighCard.hand_value();
    (c + cards[0].rank.rank_value(), m)
}

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
