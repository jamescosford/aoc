

pub mod day_07 {
  use crate::common::common::read_lines;
  use itertools::Itertools;
  use std::{str::FromStr, cmp::Ordering};
  use regex::Regex;
  use core::fmt::Formatter;

  enum Card {
    J,_2,_3,_4,_5,_6,_7,_8,_9,T,Q,K,A
  }
  fn card_from_char(s: &char) -> Option<Card> {
    match s {
      '2' => Some(Card::_2),
      '3' => Some(Card::_3),
      '4' => Some(Card::_4),
      '5' => Some(Card::_5),
      '6' => Some(Card::_6),
      '7' => Some(Card::_7),
      '8' => Some(Card::_8),
      '9' => Some(Card::_9),
      'T' => Some(Card::T),
      'J' => Some(Card::J),
      'Q' => Some(Card::Q),
      'K' => Some(Card::K),
      'A' => Some(Card::A),
      _ => None
    }
  }
  fn card_to_char(t: &Card) -> char {
    match t {
      Card::A => 'm',
      Card::K => 'l',
      Card::Q => 'k',
      Card::T => 'j',
      Card::_9 => 'i',
      Card::_8 => 'h',
      Card::_7 => 'g',
      Card::_6 => 'f',
      Card::_5 => 'e',
      Card::_4 => 'd',
      Card::_3 => 'c',
      Card::_2 => 'b',
      Card::J => 'a',
    }
  }

  #[derive(PartialEq, Eq, Clone)]
  enum Class {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfKind,
    FullHouse,
    FourOfKind,
    FiveOfKind
  }
  impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
      let cl_str = match self {
        Self::HighCard => "HighCard",
        Self::OnePair  => "OnePair",
        Self::TwoPair  => "TwoPair",
        Self::ThreeOfKind => "ThreeOfKind",
        Self::FullHouse => "FullHouse",
        Self::FourOfKind => "FourOfKind",
        Self::FiveOfKind => "FiveOfKind"
      };

      write!(f, "{}", cl_str)
    }
  }
  impl PartialOrd for Class {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      let (ai, bi) = (self.clone() as isize, other.clone() as isize);
      if ai == bi {
        Some(Ordering::Equal)
      } else if ai > bi {
        Some(Ordering::Greater)
      } else {
        Some(Ordering::Less)
      }
    }
  }

  struct Hand {
    bid: u32,
    class: Class,
    lex_order: String,
    orig: String
  }
  impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
      let (a, b) = (self, other);
      a.orig == b.orig
    }
  }

  impl Eq for Hand {}

  impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
      let (a, b) = (self, other);

      if a.orig == b.orig {
        Some(Ordering::Equal)
      } else if a.class > b.class {
        Some(Ordering::Greater)
      } else if a.class == b.class && a.lex_order > b.lex_order {
        Some(Ordering::Greater)
      } else {
        Some(Ordering::Less)
      }
    }
  }

  impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
      self.partial_cmp(other).unwrap()
    }
  }

  fn class_from_cards(cards: &str) -> Class {
    fn reps(r: char) -> String {
      //  2 3 4 5 6 7 8 9 T J Q K A
      // ( | | | | | | | | | | | | )
      let inter = "(||||||||||||)".chars().map(|v| v.to_string()).collect_vec();
      let cards  = "23456789TJQKA".chars();
      let to_inter = cards.map(|c| {
        String::from_iter(vec![c,'{', r, '}'].iter())
      }).collect_vec();
      inter.iter().interleave(to_inter.iter()).cloned().collect::<String>()
    }

    let r3 = reps('3');
    let r2 = reps('2');

    let five_of_kind_r = Regex::from_str(reps('5').as_str()).unwrap();
    let four_of_kind_r = Regex::from_str(reps('4').as_str()).unwrap();
    let full_house_r = {
      let s = vec!["(", r3.as_str(), r2.as_str(), ")|(", r2.as_str(), r3.as_str(), ")"].iter().cloned().collect::<String>();
      Regex::from_str(s.as_str()).unwrap()
    };
    let three_of_kind_r = Regex::from_str(reps('3').as_str()).unwrap();
    let two_pair_r = {
      let s = vec![r2.as_str(), ".?", r2.as_str()].iter().cloned().collect::<String>();
      Regex::from_str(s.as_str()).unwrap()
    };
    let one_pair_r = Regex::from_str(r2.as_str()).unwrap();

    // remove J's
    let sorted = cards.chars().filter(|c| *c != 'J').sorted().collect::<String>();
    let n_js = cards.chars().filter(|c| *c == 'J').count();

    let class =
      if five_of_kind_r.is_match(&sorted) {
        Class::FiveOfKind
      } else if four_of_kind_r.is_match(&sorted) {
        if n_js == 1 {
          Class::FiveOfKind 
        } else {
          Class::FourOfKind
        }
      } else if full_house_r.is_match(&sorted) {
        Class::FullHouse
      } else if three_of_kind_r.is_match(&sorted) {
        if n_js == 0 {
          Class::ThreeOfKind
        } else if n_js == 1 {
          Class::FourOfKind
        } else { //  n_js == 2
          Class::FiveOfKind
        }
      } else if two_pair_r.is_match(&sorted) {
        if n_js == 0 {
          Class::TwoPair
        } else if n_js == 1 {
          Class::FullHouse
        } else { // n_js == 2
          Class::FourOfKind
        }
      } else if one_pair_r.is_match(&sorted) {
        if n_js == 1 {
          Class::ThreeOfKind
        } else if n_js == 2 {
          Class::FourOfKind
        } else if n_js == 3 {
          Class::FiveOfKind
        } else {
          Class::OnePair
        }
      } else {
        if n_js == 5 || n_js == 4 {
          Class::FiveOfKind
        } else if n_js == 3 {
          Class::FourOfKind
        } else if n_js == 2 {
          Class::ThreeOfKind
        } else if n_js == 1 {
          Class::OnePair
        } else {
          Class::HighCard
        }
      };

    class
  }

  fn hand_from_line(l: &str) -> Hand {
    let chars = l.chars().collect_vec();
    let cards = (0usize..5)
      .flat_map(|i| chars.get(i))
      .flat_map(card_from_char)
      .collect_vec();

    let split_vec = l.split(" ").collect_vec();
    let bid_str = split_vec.get(1).unwrap();
    let bid = bid_str.parse::<u32>().unwrap();
    let class = class_from_cards(split_vec.get(0).unwrap());
    let lex = cards.iter().map(card_to_char).collect::<String>();

    Hand {
      bid: bid,
      class: class,
      lex_order: lex,
      orig: String::from_str(split_vec.get(0).unwrap()).unwrap()
    }
  }

  fn parse(path: &str) -> Vec<Hand> {
    read_lines(path).iter().map(|l| hand_from_line(l.as_str())).collect_vec()
  }

  pub fn aoc_a() -> u64 {
    parse("input/day_07.txt").iter().sorted().enumerate().map(|(i, v)| {
      ((i + 1) as u64) * (v.bid as u64)
    }).sum()
  }
}