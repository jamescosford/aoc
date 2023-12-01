

pub mod day_2023_01 {
  use crate::common::common::read_lines;
  use itertools::Itertools;
  use regex::Regex;
  use std::{collections::HashMap, str::FromStr};

  pub fn aoc_a() -> i32 {
    let re = Regex::new(r"(\d)").unwrap();

    read_lines("input/day_2023_01.txt")
      .iter()
      .map(|f| {
        let caps = re.captures_iter(f).collect_vec();

        let df = caps.first().map(|c| {
          let c1 = &c[1];
          c1.parse::<i32>().unwrap()
        }).unwrap();
        
        let dr = caps.last().map(|c| {
          let c1 = &c[1];
          c1.parse::<i32>().unwrap()
        }).unwrap();

        df*10 + dr
      }).fold(0, |acc, v| acc + v)
  }

  pub fn aoc_b() -> i32 {

    let lut_f = HashMap::from([
      ("1", 1),
      ("2", 2),
      ("3", 3),
      ("4", 4),
      ("5", 5),
      ("6", 6),
      ("7", 7),
      ("8", 8),
      ("9", 9),
      ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9),
    ]);

    let nums = vec![r"1", "2", "3", "4", "5", "6", "7", "8", "9", "one", "two", "three", "four","five","six","seven","eight","nine"];

    let regexes: HashMap<&&str, Regex> = nums.iter().map(|s| (s,  Regex::from_str(s).unwrap())).collect();

    read_lines("input/day_2023_01.txt")
      .iter()
      .map(|f| {
        let res: (Option<(usize, i32)>, Option<(usize, i32)>) = nums.iter().fold((None, None), |acc: (Option<(usize, i32)>, Option<(usize, i32)>), v| {
          let re = regexes.get(v).unwrap();
          let finds = re.find_iter(f).collect_vec();
          let (acc_f, acc_l) = acc;

          let upd_f = match (acc_f, finds.first().map(|v| v.start())) {
            (Some((p,_)),Some(new_p)) if new_p < p => Some((new_p, *lut_f.get(v).unwrap())),
            (None, Some(new_p)) => Some((new_p, *lut_f.get(v).unwrap())),
            _ => acc_f
          };

          let upd_l = match (acc_l, finds.last().map(|v| v.start())) {
            (Some((p,_)), Some(new_p)) if new_p > p => Some((new_p, *lut_f.get(v).unwrap())),
            (None, Some(new_p)) => Some((new_p, *lut_f.get(v).unwrap())),
            _ => acc_l
          };

          (upd_f, upd_l)
        });

        res.0.unwrap().1 * 10 + res.1.unwrap().1

      }).fold(0, |acc, v| acc + v)

  }
}
