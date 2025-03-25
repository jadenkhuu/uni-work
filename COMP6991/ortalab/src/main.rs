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
use ortalib::{Rank};

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
    println!("Cards played: {:?}", round.cards_played);
    println!("Cards In hand: {:?}", round.cards_held_in_hand);
    // println!("{:?}", cards_by_rank);
    // println!("{:?}", cards_by_suit);

    if check_straightflush(&cards_by_rank) {
        println!("Straight Flush");
        five_card_calc(&cards_by_rank, PokerHand::StraightFlush)
    }
    else if check_quad(&cards_by_rank) {
        println!("Four of a Kind");
        five_card_calc(&cards_by_rank, PokerHand::FourOfAKind)
    }
    else if check_fullhouse(&cards_by_rank) {
        println!("Fullhouse");
        five_card_calc(&cards_by_rank, PokerHand::FullHouse)
    }
    else if check_flush(&cards_by_suit) {
        println!("Flush");
        five_card_calc(&cards_by_suit, PokerHand::Flush)
    }
    else if check_straight(&cards_by_rank) {
        println!("Straight");
        five_card_calc(&cards_by_rank, PokerHand::Straight)
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
    // Straight Flush x
    // Four of a Kind x
    // Full House x
    // Flush x
    // Straight x
    // Three of a Kind x
    // Two Pair x
    // Pair x
    // High Card x

}

// GENERIC CALCULATION FOR FULL 5 CARD HAND //
fn five_card_calc(cards: &[Card], hand_type: PokerHand) -> (f64, f64) {
    let (mut c, m) = hand_type.hand_value();
    for i in 0..cards.len() {
        c = c + cards[i].rank.rank_value();
    }
    (c, m)
}

// STRAIGHT FLUSH //
fn check_straightflush(cards: &[Card]) -> bool {
    check_straight(cards) && check_flush(cards)
}

// Four of a Kind //
// Checks for Four of a Kind
fn check_quad(cards: &[Card]) -> bool {
    cards.windows(4)
        .any(|pair| pair[0].rank == pair[1].rank && pair[1].rank == pair[2].rank)
}

// FULL HOUSE //
// Checks for Full House
fn check_fullhouse(cards: &[Card]) -> bool {
    check_triple(cards) && check_pair(cards)
}

// FLUSH //
// Checks for Flush
fn check_flush(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }

    for i in 0..cards.len() - 1 {
        if cards[i].suit != cards[i + 1].suit {
            return false;
        }
    }
    true
}

// STRAIGHT //
// Checks for Straight
fn check_straight(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }

    let mut start = 0;
    // Edge case for Ace Low Straight
    if rank_to_num(&cards[0].rank) == 14 {
        start = 1;
    }
    for i in start..cards.len() - 1 {
        if rank_to_num(&cards[i].rank) != rank_to_num(&cards[i + 1].rank) + 1 {
            return false;
        }
    }
    true

    // ace can only be at the start or end of a straight, consider edge case for A,2,3,4,5
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

    (c + pairs[0].rank.rank_value() * 3.0, m)
}

// TWO PAIR //
// Checks for Two Pair | Calculate (Chips, Mult) of a Two Pair
fn check_twopair(cards: &[Card]) -> bool {
    let pairs = cards.windows(2)
    .filter(|pair| pair[0].rank == pair[1].rank) // Find pairs with the same rank
    .map(|pair| pair[0]) // Extract the rank of each pair
    .collect::<Vec<_>>(); // Collect ranks of pairs into a vector
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

    (c + paired_card.rank.rank_value() * 2.0, m)
}

// HIGH CARD //
// Calculates (Chips, Mult) of a High Card hand
fn calc_highcard(cards: &[Card]) -> (f64, f64) {
    let (c, m) = PokerHand::HighCard.hand_value();
    (c + cards[0].rank.rank_value(), m)
}


// HELPER FUNCTIONS //
// Sorts in descending order by rank
fn sorted_by_rank(round: &Round) -> Vec<Card> {
    round.cards_played.iter()
        .sorted_by_key(|card| std::cmp::Reverse(rank_to_num(&card.rank)))
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

// Takes the rank of a card and provides a numerical value
fn rank_to_num(rank: &Rank) -> i32 {
    match rank {
        Rank::Two => 2,
        Rank::Three => 3,
        Rank::Four => 4,
        Rank::Five => 5,
        Rank::Six => 6,
        Rank::Seven => 7,
        Rank::Eight => 8,
        Rank::Nine => 9,
        Rank::Ten => 10,
        Rank::Jack => 11,
        Rank::Queen => 12,
        Rank::King => 13,
        Rank::Ace => 14,
    }
}
