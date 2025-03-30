use std::{
    collections::{HashMap, HashSet}, error::Error, fs::File, io::{stdin, Read}, path::{Path, PathBuf}
};

use itertools::Itertools;

use clap::Parser;
use ortalib::{
    Card, Chips, Edition, Enhancement, Joker, Mult, PokerHand, Rank, Round, Suit
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
    // 1.0 Determine what the combo hand is
    let hand_type = check_hand(&round);

    // 1.0 create a subset of cards valid to the combo
    let made_hand  = make_hand(&round, hand_type);

    // 1.0 Calculate the base amount for the PLAYED HAND base on the combo
    let mut score = hand_type.hand_value();

    // 2.1, 2.2, 2.3
    // Score each card, if splash is active, use round.played_cards instead of made_hand
    // 2.4 "On scored" jokers
    score = score_cards(&made_hand, score, &round);


    // 3.1 Check cards held in hand for enhancements
    // 3.2 Activates "On Held" Jokers too
    score = score_held_cards(&round, score);

    // 4.1 Applies joker editions, ONLY Foil or Holographic
    score = score_joker_editions(&round, score);

    // 4.2 activate INDEPENDENT jokers
    // 4.3 Applies Joker Polychrome edition
    score = activate_independent_jokers_polychrome(&round, score);

    score
}

// Applies only Foil or Holographic editions
// 4.1
fn score_joker_editions(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    for joker in &round.jokers {
        if let Some(edition) = &joker.edition {
            match edition {
                Edition::Foil => score.0 += 50.0,
                Edition::Holographic => score.1 += 10.0,
                _ => {},
            }
        }
    }
    score
}

// Activates independent jokers, then applies Polychrome editions for jokers.
// 4.2, 4.3
fn activate_independent_jokers_polychrome(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    let played_by_rank = sorted_by_rank(&round.cards_played);
    let played_by_suit = sorted_by_suit(&round.cards_played);
    for joker in &round.jokers {
        match joker.joker {
            Joker::Joker => score.1 += 4.0,
            Joker::JollyJoker => {
                if check_pair(&played_by_rank) {
                    score.1 += 8.0;
                }},
            Joker::ZanyJoker => {
                if check_triple(&played_by_rank) {
                    score.1 += 12.0;
                }},
            Joker::MadJoker => {
                if check_twopair(&played_by_suit) {
                    score.1 += 10.0;
                }},
            Joker::CrazyJoker => {
                if check_straight(&played_by_rank) {
                    score.1 += 12.0;
                }},
            Joker::DrollJoker => {
                if check_flush(&played_by_suit) {
                    score.1 += 10.0;
                }},
            Joker::SlyJoker => {
                if check_pair(&played_by_rank) {
                    score.0 += 50.0;
                }},
            Joker::WilyJoker => {
                if check_triple(&played_by_rank) {
                    score.0 += 100.0;
                }},
            Joker::CleverJoker => {
                if check_twopair(&played_by_suit) {
                    score.0 += 80.0;
                }},
            Joker::DeviousJoker => {
                if check_straight(&played_by_rank) {
                    score.0 += 100.0;
                }},
            Joker::CraftyJoker => {
                if check_flush(&played_by_suit) {
                    score.0 += 80.0;
                }},
            Joker::AbstractJoker => {
                score.1 += 3.0 * round.jokers.len() as f64;
            },
            Joker::Blackboard => {
                if round.cards_held_in_hand.iter().all(|card|
                    card.suit == Suit::Clubs ||
                    card.suit == Suit::Spades ||
                    card.enhancement == Some(Enhancement::Wild))
                {
                    score.1 *= 3.0;
                }
            }
            Joker::FlowerPot => {
                if played_by_suit.len() >= 4 && at_least_one_each_suit(&played_by_rank) {
                    score.1 *= 3.0;
                }
            }
            _ => {},
        }
    }
    for joker in &round.jokers {
        if let Some(edition) = &joker.edition {
            match edition {
                Edition::Polychrome => score.1 *= 1.5,
                _ => {},
            }
        }
    }

    score
}

// Flower Pot Joker helper function
// Checks if the hand has at least one of each suit or wildcard
fn at_least_one_each_suit(cards: &[Card]) -> bool {
    let mut suits_seen = HashSet::new();
    let mut wild_cards = 0;

    for card in cards {
        if let Some(enhancement) = card.enhancement {
            if enhancement == Enhancement::Wild {
                wild_cards += 1;
            }
        } else {
            suits_seen.insert(card.suit.clone());
        }
    }

    // return final bool accounting for wild cards too.
    suits_seen.len() + wild_cards >= 4
}

// HAND OPERATIONS //
// Returns the Poker Hand ID of
fn check_hand(round: &Round) -> PokerHand {
    let cards_by_rank = &sorted_by_rank(&round.cards_played);
    let cards_by_suit = &sorted_by_suit(&round.cards_played);

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

// Determines the best poker hand possible, returns subset of the hand as valid cards
// 1.0
fn make_hand(round: &Round, hand_type: PokerHand) -> Vec<Card> {
  // create a subset of cards valid to the combo
    let cards_by_rank = sorted_by_rank(&round.cards_played);
    let cards_by_suit = sorted_by_suit(&round.cards_played);

    let made_hand: Vec<Card> = match hand_type {
        PokerHand::FlushFive => cards_by_suit,
        PokerHand::FlushHouse => cards_by_suit,
        PokerHand::FiveOfAKind => cards_by_suit,
        PokerHand::StraightFlush => cards_by_suit,
        PokerHand::FourOfAKind => calc_duplicates(&cards_by_suit, 4),
        PokerHand::FullHouse => cards_by_suit,
        PokerHand::Flush => cards_by_rank,
        PokerHand::Straight => cards_by_rank,
        PokerHand::ThreeOfAKind => calc_duplicates(&cards_by_suit, 3),
        PokerHand::TwoPair => calc_twopair(&cards_by_suit),
        PokerHand::Pair => calc_duplicates(&cards_by_suit, 2),
        _ => calc_highcard(&cards_by_rank),
    };
    made_hand
}

// HAND SCORING //
// Iterates through each card in played combo to score cards
// 2.1, 2.2, 2.3
fn score_cards(cards: &[Card], mut score: (Chips, Mult), round: &Round) -> (Chips, Mult) {
    let first_face_card = cards.iter().find(|card| card.rank.is_face());
    let has_photograph_joker = round.jokers.iter().any(|j| matches!(j.joker, Joker::Photograph));

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

        // If the player has Photograph Joker and it is the first face card
        if Some(card) == first_face_card && has_photograph_joker {
            score.1 *= 2.0;
        }

        score = activate_on_scored_jokers(card, score, round);
    }

    score
}
// 2.4 On scored jokers helper
fn activate_on_scored_jokers(card: &Card, mut score: (Chips, Mult), round: &Round) -> (Chips, Mult) {
    for joker in &round.jokers {
        match joker.joker {
            Joker::GreedyJoker => {
                if card.suit == Suit::Diamonds ||
                    card.enhancement == Some(Enhancement::Wild)
                {
                    score.1 += 3.0;
                }},
            Joker::LustyJoker => {
                if card.suit == Suit::Hearts ||
                    card.enhancement == Some(Enhancement::Wild)
                {
                    score.1 += 3.0;
                }},
            Joker::WrathfulJoker => {
                if card.suit == Suit::Spades ||
                    card.enhancement == Some(Enhancement::Wild)
                {
                    score.1 += 3.0;
                }},
            Joker::GluttonousJoker => {
                if card.suit == Suit::Clubs ||
                    card.enhancement == Some(Enhancement::Wild)
                {
                    score.1 += 3.0;
                }},
            Joker::Fibonacci => {
                if card.rank == Rank::Ace ||
                    card.rank == Rank::Two ||
                    card.rank == Rank::Three ||
                    card.rank == Rank::Five ||
                    card.rank == Rank::Eight
                {
                    score.1 += 8.0;
                }},
            Joker::ScaryFace => {
                if card.rank.is_face() {
                    score.0 += 30.0;
                }},
            Joker::EvenSteven => {
                if !card.rank.is_face() && card.rank.rank_value() % 2.0 == 0.0 {
                    score.1 += 4.0;
                }},
            Joker::OddTodd => {
                if !card.rank.is_face() && card.rank.rank_value() % 2.0 != 0.0 {
                    score.0 += 31.0;
                }},
            Joker::SmileyFace => {
                if card.rank.is_face() {
                    score.1 += 5.0;
                }},
            _ => {},
        }
    }
    score
}

// 3.1 Check cards in hand, apply enhancements
// 3.2 Activates "On Held" jokers
fn score_held_cards(round: &Round, mut score: (Chips, Mult)) -> (Chips, Mult) {
    let has_raised_fist_joker = round.jokers.iter().any(|j| matches!(j.joker, Joker::RaisedFist));

    for card in &round.cards_held_in_hand {
        if let Some(enhancement) = &card.enhancement {
            match enhancement {
                Enhancement::Steel => score.1 *= 1.50,
                _ => {},
            }
        }
        score = activate_on_held_jokers(card, score, round);
    }

    if has_raised_fist_joker {
        let min_rank = round.cards_held_in_hand.iter().map(|c| c.rank).min().unwrap();
        let lowest_rank = round.cards_held_in_hand.iter().rfind(|c| c.rank == min_rank).unwrap();
        score.1 += lowest_rank.rank.rank_value() * 2.0;
    }

    score
}
fn activate_on_held_jokers(card: &Card, mut score: (Chips, Mult), round: &Round) -> (Chips, Mult) {
    for joker in &round.jokers {
        match joker.joker {
            // Joker::RaisedFist => {},
            Joker::Baron => {
                if card.rank == Rank::King {
                    score.1 *= 1.5;
                }
            },
            _ => {}
        }
    }
    score
}


// POKER COMBO CHECKS //
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
    // Consists of exactly triple and double
    duplicate_cards(cards).values().any(|&count| count == 3)
    && duplicate_cards(cards).values().any(|&count| count == 2)
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
    if rank_to_num(&cards[0].rank) == 14 && cards[1].rank == Rank::Five{
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
    duplicate_cards(cards).values().any(|&count| count >= 3)
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
    duplicate_cards(cards).values().any(|&count| count >= 2)
}

// HIGH CARD CALCULATION //
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
fn sorted_by_rank(cards: &[Card]) -> Vec<Card> {
    cards.iter()
        .sorted_by_key(|card| std::cmp::Reverse(rank_to_num(&card.rank)))
        .cloned() // convert back to Vec<Card>
        .collect()
}

// Sorts and groups into suits (spade, heart, club, diamond) while keeping rank order
fn sorted_by_suit(cards: &[Card]) -> Vec<Card> {
    cards.iter()
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
