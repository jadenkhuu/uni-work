use std::{
    collections::HashMap, error::Error, fs::File, io::{stdin, Read}, path::{Path, PathBuf}
};

use itertools::Itertools;

use clap::Parser;
use ortalib::{
    Card, Chips, Edition, Enhancement, Joker, Mult, PokerHand, Rank, Round
};

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
    // Determine what the combo hand is
    let hand_type = check_hand(&round);

    // create a subset of cards valid to the combo
    let made_hand  = make_hand(&round, hand_type);

    // Calculate the base amount for the PLAYED HAND base on the combo
    let mut score = hand_type.hand_value();
    // println!("{:?}", score);

    // Score each card, if splash is active, use round.played_cards instead of made_hand
    score = score_cards(&made_hand, score);
    // println!("{:?}", score);

    // Check cards held in hand for enhancements
    score = score_held_cards(&round, score);
    // println!("{:?}", score);

    // activate INDEPENDENT jokers
    score = activate_independent_jokers(&round, score);
    // println!("{:?}", score);
    // score joker editions,
    // println!("{:?}", score);
    score = score_joker_editions(&round, score);

    score
}

// SCORING JOKER EDITIONS
fn score_joker_editions(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    for joker in &round.jokers {
        if let Some(edition) = &joker.edition {
            match edition {
                Edition::Foil => score.0 += 50.0,
                Edition::Holographic => score.1 += 10.0,
                Edition::Polychrome => score.1 *= 1.5,
            }
        }
    }
    score
}

// INDEPENDENT JOKERS
fn activate_independent_jokers(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    for joker in &round.jokers {
        match joker.joker {
            Joker::Joker => score.1 += 4.0,
            Joker::JollyJoker => {
                if duplicate_cards(&round.cards_played).values().any(|&count| count >= 2) {
                    score.1 += 8.0;
                }},
            Joker::ZanyJoker => {
                if duplicate_cards(&round.cards_played).values().any(|&count| count >= 3) {
                    score.1 += 12.0;
                }},
            Joker::MadJoker => {
                if duplicate_cards(&round.cards_played).values().filter(|&&count| count >= 2).count() == 2 {
                    score.1 += 10.0;
                }},
            Joker::CrazyJoker => {
                if check_straight(&sorted_by_rank(&round)) {
                    score.1 += 12.0;
                }},
            Joker::DrollJoker => {
                if check_flush(&sorted_by_suit(&round)) {
                    score.1 += 10.0;
                }},
            Joker::SlyJoker => {
                if duplicate_cards(&round.cards_played).values().any(|&count| count >= 2) {
                    score.0 += 50.0;
                }},
            Joker::WilyJoker => {
                if duplicate_cards(&round.cards_played).values().any(|&count| count >= 3) {
                    score.0 += 100.0;
                }},
            Joker::CleverJoker => {
                if duplicate_cards(&round.cards_played).values().filter(|&&count| count >= 2).count() == 2 {
                    score.0 += 80.0;
                }},
            Joker::DeviousJoker => {
                if check_straight(&sorted_by_rank(&round)) {
                    score.0 += 100.0;
                }},
            Joker::CraftyJoker => {
                if check_flush(&sorted_by_suit(&round)) {
                    score.0 += 80.0;
                }},
            Joker::AbstractJoker => {
                score.1 += 3.0 * round.jokers.len() as f64;
            },
            _ => {},
        }
    }

    score
}


// Returns the Poker Hand ID of
fn check_hand(round: &Round) -> PokerHand {
    let cards_by_rank = &sorted_by_rank(round);
    let cards_by_suit = &sorted_by_suit(round);

    if check_flushfive(&cards_by_rank) {
        // println!("Flush Five");
        PokerHand::FlushFive
    }
    else if check_flushhouse(&cards_by_rank) {
        // println!("Full House");
        PokerHand::FlushHouse
    }
    else if check_five(&cards_by_rank) {
        // println!("Five of a Kind");
        PokerHand::FiveOfAKind
    }
    else if check_straightflush(&cards_by_rank) {
        // println!("Straight Flush");
        PokerHand::StraightFlush
    }
    else if check_quad(&cards_by_rank) {
        // println!("Four of a Kind");
        PokerHand::FourOfAKind
    }
    else if check_fullhouse(&cards_by_rank) {
        // println!("Fullhouse");
        PokerHand::FullHouse
    }
    else if check_flush(&cards_by_suit) {
        // println!("Flush");
        PokerHand::Flush
    }
    else if check_straight(&cards_by_rank) {
        // println!("Straight");
        PokerHand::Straight
    }
    else if check_triple(&cards_by_rank) {
        // println!("Three of a Kind");
        PokerHand::ThreeOfAKind
    }
    else if check_twopair(&cards_by_rank) {
        // println!("Two Pair");
        PokerHand::TwoPair
    }
    else if check_pair(&cards_by_rank) {
        // println!("Pair");
        PokerHand::Pair
    }
    else {
        // println!("High card");
        PokerHand::HighCard
    }
}

// DETERMINES POKER COMBO, CREATES A SUBSET CARDS BASED ON COMBO
fn make_hand(round: &Round, hand_type: PokerHand) -> Vec<Card> {
  // create a subset of cards valid to the combo

    let made_hand: Vec<Card> = match hand_type {
        PokerHand::FlushFive => sorted_by_suit(&round),
        PokerHand::FlushHouse => sorted_by_suit(&round),
        PokerHand::FiveOfAKind => sorted_by_suit(&round),
        PokerHand::StraightFlush => sorted_by_suit(&round),
        PokerHand::FourOfAKind => calc_duplicates(&sorted_by_suit(&round), 4),
        PokerHand::FullHouse => sorted_by_suit(&round),
        PokerHand::Flush => sorted_by_rank(&round),
        PokerHand::Straight => sorted_by_rank(&round),
        PokerHand::ThreeOfAKind => calc_duplicates(&sorted_by_suit(&round), 3),
        PokerHand::TwoPair => calc_twopair(&sorted_by_suit(&round)),
        PokerHand::Pair => calc_duplicates(&sorted_by_suit(&round), 2),
        _ => calc_highcard(&sorted_by_rank(&round)),
    };
    made_hand
}

// iterates through each card in played combo to score cards
fn score_cards(cards: &[Card], mut score: (Chips, Mult)) -> (Chips, Mult) {
    for card in cards {
        score.0 += card.rank.rank_value();
        if let Some(enhancement) = &card.enhancement {
            match enhancement {
                Enhancement::Bonus => score.0 += 30.0,
                Enhancement::Mult => score.1 += 4.0,
                Enhancement::Glass => score.1 *= 2.0,
                _ => {},
            }
        }
        if let Some(edition) = &card.edition {
            match edition {
                Edition::Foil => score.0 += 50.0,
                Edition::Holographic => score.1 += 10.0,
                Edition::Polychrome => score.1 *= 1.5,
            }
        }
    }

    score
}
// iterates through each card in hand to score cards
fn score_held_cards(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    for card in &round.cards_held_in_hand {
        if let Some(enhancement) = &card.enhancement {
            match enhancement {
                Enhancement::Steel => score.1 *= 1.50,
                _ => {},
            }
        }

        // if let Some(edition) = &card.edition {
        //     match edition {
        //         Edition::Foil => score.0 += 50.0,
        //         Edition::Holographic => score.1 += 10.0,
        //         _ => {},
        //     }
        // }
    }
    score
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
    duplicate_cards(cards).values().any(|&count| count >= 4)
}

// FULL HOUSE //
fn check_fullhouse(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }
    check_triple(cards) && check_pair(cards)
}

// FLUSH //
fn check_flush(cards: &[Card]) -> bool {
    if cards.len() < 5 { return false; }

    // counts most often suit (excluding wilds)
    let mut suit_counts = HashMap::new();
    let mut wild_count = 0;

    for card in cards {
        if card.enhancement == Some(Enhancement::Wild) {
            wild_count += 1;
        } else {
            *suit_counts.entry(card.suit).or_insert(0) += 1;
        }
    }

    // finds the most common suit
    if let Some((_most_common_suit, &count)) = suit_counts.iter().max_by_key(|&(_, &count)| count) {
        return count + wild_count >= 5;
    }
    wild_count >= 5
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
fn calc_twopair(cards: &[Card]) -> Vec<Card> {
    let mut rank_counts: HashMap<Rank, Vec<Card>> = HashMap::new();

    // Group cards by rank
    for card in cards {
        rank_counts.entry(card.rank).or_insert(Vec::new()).push(card.clone());
    }

    // Collect pairs
    let mut pairs = Vec::new();
    for (_, group) in rank_counts {
        if group.len() == 2 { // If exactly two cards of the same rank exist, it's a pair
            pairs.extend(group);
        }
    }

    pairs
}
// PAIR //
fn check_pair(cards: &[Card]) -> bool {
    duplicate_cards(cards).values().any(|&count| count == 2)
}

// HIGH CARD //
fn calc_highcard(cards: &[Card]) -> Vec<Card> {
    vec![cards[0].clone()]
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
fn calc_duplicates(cards: &[Card], num: usize) -> Vec<Card> {
    let Some(paired_rank) = cards.iter()
        .map(|card| card.rank)
        .find(|&rank| cards.iter().filter(|card| card.rank == rank).count() >= num)
    else {
        return Vec::new();
    };

    cards.iter()
        .filter(|card| card.rank == paired_rank)
        .cloned()
        .collect()
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
