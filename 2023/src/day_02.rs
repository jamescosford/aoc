pub mod day_02 {
  use crate::common::common::read_lines;
  use itertools::Itertools;
  use regex::Regex;
  use std::str::FromStr;
  use std::fmt::Formatter;

  enum Colour {
    Blue,
    Red,
    Green,
    Fuck
  }

  impl std::fmt::Display for Colour {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
      let str = match self {
        Colour::Blue => "blue",
        Colour::Red => "red",
        Colour::Green => "green",
        Colour::Fuck => "FUCK!"
      };
      write!(f, "{}", str)
    }
  }

  type Hand = Vec<(Colour, i32)>;

  type Game = (i32, Vec<Hand>);
 
  
  fn games() -> Vec<(i32, Vec<Vec<(Colour, i32)>>)> {
    let game_re = Regex::from_str(r"Game (\d+): (.*)$").unwrap();
    let single_re = Regex::from_str(r"\s*(\d+) (red|blue|green),?").unwrap();
    read_lines("input/day_02.txt")
      .iter()
      .flat_map(|l| {
        game_re.captures_iter(l).map(|v| {          
          let gid = v[1].parse::<i32>().unwrap();
          let hands = v[2].split(";").map(|v| {
            single_re.captures_iter(v).map(|v| {
              let n = v[1].parse::<i32>().unwrap();
              let colour = match &v[2] {
                "red" => Colour::Red,
                "green" => Colour::Green,
                "blue" => Colour::Blue,
                _ => Colour::Fuck
              };
              (colour, n)
            }).collect_vec() 
          }).collect_vec();
          (gid, hands)
        }).collect_vec()
      }).collect_vec()
  }

  pub fn aoc_a() -> i32 {

    fn decide_hand(hand: &Hand) -> bool {
      // 12 red, 13 green, 14 blue 
      hand.iter().fold(true, |acc, v| {
        acc && match v.0 {
          Colour::Red   if v.1 > 12 => false,
          Colour::Green if v.1 > 13 => false,
          Colour::Blue  if v.1 > 14 => false,
          _ => true
        }
      })
    }

    fn decide_game(game: &Game) -> i32 {
      match game.1.iter().fold(true, |acc, v| {
        acc && decide_hand(v)
      }) {
        true => game.0,
        _ => 0
      }
    }

    let games = games();

    games.iter().map(|game| decide_game(game)).fold(0i32, |acc, v| acc + v)
  }

  pub fn aoc_b() -> i64 {
    fn size_hand(h: &Hand) -> (i64, i64, i64) {
      h.iter().fold((0i64,0i64,0i64), |acc, v| {
        match v.0 {
          Colour::Red => (v.1 as i64, acc.1, acc.2),
          Colour::Green => (acc.0, v.1 as i64, acc.2),
          Colour::Blue => (acc.0, acc.1, v.1 as i64),
          Colour::Fuck => acc
        }
      })
    }

    fn size_game(g: &Game) -> (i64, i64, i64) {
      g.1.iter().map(size_hand).fold((0i64,0i64,0i64), |acc, v| {
        (
          if v.0 > acc.0 { v.0 } else { acc.0 },
          if v.1 > acc.1 { v.1 } else { acc.1 },
          if v.2 > acc.2 { v.2 } else { acc.2 }
        )
      })
    }
  
    let games = games();

    games.iter().map(size_game).map(|v| {
      v.0 * v.1 * v.2
    }).fold(0i64, |acc, v| acc + v)
  }
}
