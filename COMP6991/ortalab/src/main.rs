use std::{
    collections::HashMap, error::Error, fs::File, io::{stdin, Read}, ops::Mul, path::{Path, PathBuf}
};

use itertools::Itertools;
// use std::collections::HashMap;

use clap::Parser;
use ortalib::{Chips, Mult, Round, PokerHand, Card, Rank, Enhancement};
use serde_yaml::Number;

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
    // make_hand(&round)
    let mut score = make_hand(&round);
    score = apply_enhancements(&round, score);
    score = apply_editions(&round, score);
    score
}

fn apply_enhancements(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    for card in &round.cards_played {
        if let Some(enhancement) = &card.enhancement {
            match enhancement {
                Enhancement::Bonus => score.0 += 30.0,
                Enhancement::Mult => score.1 += 4.0,
                Enhancement::Glass => score.1 += 2.0,
                Enhancement::Steel => score.1 += 1.5,
                _ => {}
            }
        }
    }
    score
}

fn apply_editions(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    todo!()
}

// Takes the cards_played and finds what poker hand it is
// Calculates hand value and returns
fn make_hand(round: &Round) -> (Chips, Mult) {
    let cards_by_rank = &sorted_by_rank(round);
    let cards_by_suit = &sorted_by_suit(round);

    // Debugging prints
    // println!("Cards played: {:?}", round.cards_played);
    // println!("Cards In hand: {:?}", round.cards_held_in_hand);
    // println!("by rank {:?}", cards_by_rank);
    // println!("by suit {:?}", cards_by_suit);

    if check_flushfive(&cards_by_rank) {
        // println!("Flush Five");
        calc_five_cards(&cards_by_rank, PokerHand::FlushFive)
    }
    else if check_flushhouse(&cards_by_rank) {
        // println!("Full House");
        calc_five_cards(&cards_by_rank, PokerHand::FlushHouse)
    }
    else if check_five(&cards_by_rank) {
        // println!("Five of a Kind");
        calc_five_cards(&cards_by_rank, PokerHand::FiveOfAKind)
    }
    else if check_straightflush(&cards_by_rank) {
        // println!("Straight Flush");
        calc_five_cards(&cards_by_rank, PokerHand::StraightFlush)
    }
    else if check_quad(&cards_by_rank) {
        // println!("Four of a Kind");
        // calc_quad(&cards_by_rank)
        calc_duplicates(&cards_by_rank, 4, PokerHand::FourOfAKind)
    }
    else if check_fullhouse(&cards_by_rank) {
        // println!("Fullhouse");
        calc_five_cards(&cards_by_rank, PokerHand::FullHouse)
    }
    else if check_flush(&cards_by_suit) {
        // println!("Flush");
        calc_five_cards(&cards_by_suit, PokerHand::Flush)
    }
    else if check_straight(&cards_by_rank) {
        // println!("Straight");
        calc_five_cards(&cards_by_rank, PokerHand::Straight)
    }
    else if check_triple(&cards_by_rank) {
        // println!("Three of a Kind");
        // calc_triple(&cards_by_rank)
        calc_duplicates(&cards_by_rank, 3, PokerHand::ThreeOfAKind)
    }
    else if check_twopair(&cards_by_rank) {
        // println!("Two Pair");
        calc_twopair(&cards_by_rank)
    }
    else if check_pair(&cards_by_rank) {
        // println!("Pair");
        // calc_pair(&cards_by_rank)
        calc_duplicates(&cards_by_rank, 2, PokerHand::Pair)
    }
    else {
        // println!("High card");
        calc_highcard(&cards_by_rank)
    }
}

// FLUSH FIVE //
fn check_flushfive(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }
    check_five(cards) && check_flush(cards)
}

// FLUSH HOUSE //
fn check_flushhouse(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }
    check_fullhouse(cards) && check_flush(cards)
}

// FIVE OF A KIND //
fn check_five(cards: &[Card]) -> bool {
    duplicate_cards(cards).values().any(|&count| count == 5)
}

// STRAIGHT FLUSH //
fn check_straightflush(cards: &[Card]) -> bool {
    check_straight(cards) && check_flush(cards)
}

// FOUR OF A KIND //
fn check_quad(cards: &[Card]) -> bool {
    duplicate_cards(cards).values().any(|&count| count == 4)
}

// FULL HOUSE //
fn check_fullhouse(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }
    check_triple(cards) && check_pair(cards)
}

// FLUSH //
fn check_flush(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }
    cards
    .first()
    .map(|first_card| cards.iter().all(|card| card.suit == first_card.suit))
    .unwrap_or(false)
}

// STRAIGHT //
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
fn check_triple(cards: &[Card]) -> bool {
    duplicate_cards(cards).values().any(|&count| count == 3)
}

// TWO PAIR //
fn check_twopair(cards: &[Card]) -> bool {
    duplicate_cards(cards).values().filter(|&&count| count == 2).count() == 2
}
fn calc_twopair(cards: &[Card]) -> (Chips, Mult) {
    let (c, m) = PokerHand::TwoPair.hand_value();

    let pairs = cards.windows(2)
    .filter(|pair| pair[0].rank == pair[1].rank)
    .map(|pair| pair[0])
    .collect::<Vec<_>>();

    (c + pairs[0].rank.rank_value() * 2.0 + pairs[1].rank.rank_value() * 2.0, m)
}

// fn calc_twopair(cards: &[Card]) -> (Chips, Mult) {
//     let (c, m) = PokerHand::TwoPair.hand_value();

//     let two_pairs: Vec<_> = cards.iter()
//         .map(|card| card.rank)
//         .filter(|&rank| cards.iter().filter(|card| card.rank == rank).count() >= 2)
//         .collect();

//     let unique_pairs: Vec<_> = two_pairs.into_iter().collect::<std::collections::HashSet<_>>().into_iter().collect();

//     if unique_pairs.len() == 2 {
//         let pair_value = unique_pairs.iter().map(|&rank| rank.rank_value()).sum::<f64>();
//         return (c + pair_value * 2.0, m);
//     }

//     (0.0, m) // Or some other error handling logic if needed
// }

// PAIR //
fn check_pair(cards: &[Card]) -> bool {
    duplicate_cards(cards).values().any(|&count| count == 2)
}

// HIGH CARD //
fn calc_highcard(cards: &[Card]) -> (Chips, Mult) {
    let (c, m) = PokerHand::HighCard.hand_value();
    (c + cards[0].rank.rank_value(), m)
}

//// HELPER FUNCTIONS ////
// Puts duplicate cards in hash map to help make hand
fn duplicate_cards(cards: &[Card]) -> HashMap<i32, usize> {
    let mut card_count = HashMap::new();
    for card in cards {
        *card_count.entry(rank_to_num(&card.rank)).or_insert(0) += 1;
    }
    card_count
}

// Variable calc for duplicate cards (Eg pairs, triples, quads)
fn calc_duplicates(cards: &[Card], num: usize, hand_type: PokerHand) -> (Chips, Mult) {
    let (c, m) = hand_type.hand_value();

    let paired_rank = cards.iter()
        .map(|card| card.rank)
        .find(|&rank| cards.iter().filter(|card| card.rank == rank).count() >= num)
        .unwrap();

    (c + paired_rank.rank_value() * (num as f64), m)
}

// Generic Calculation for full 5 card hand
fn calc_five_cards(cards: &[Card], hand_type: PokerHand) -> (Chips, Mult) {
    let (mut c, m) = hand_type.hand_value();
    for i in 0..cards.len() {
        c = c + cards[i].rank.rank_value();
    }
    (c, m)
}

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
