pub mod day_04 {
  use crate::common::common::read_lines;
  use itertools::Itertools;
  use regex::Regex;
  use std::collections::{HashSet, HashMap};
  use std::ops::Range;
  use std::str::FromStr;
  use core::fmt::Formatter;
  use std::cmp::max;
  use itertools::unfold;


  fn get() -> Vec<(i32, Vec<i32>, Vec<i32>)> {
    let lines = read_lines("input/day_04.txt");
    lines.iter().map(|l| {
      let card_nums = l.split(":").collect_vec();
      let win_haves = card_nums.get(1).unwrap().split("|").collect_vec();

      let card_name = card_nums.get(0).unwrap();
      let win_num_str = win_haves.get(0).unwrap();
      let have_num_str = win_haves.get(1).unwrap();

      let card_re = Regex::from_str(r"Card +(\d+)").unwrap();

      let card_num = card_re.captures(&card_name).unwrap().get(1).unwrap().as_str().parse::<i32>().unwrap();

      fn nums_from_str(str: &str) -> Vec<i32>  {
        let nums_re = Regex::from_str(r" *(\d+) *").unwrap();
        nums_re.captures_iter(str).flat_map(|c| {
          (1..c.len()).map(|i| {
            c.get(i).unwrap().as_str().parse::<i32>().unwrap()
          }).collect_vec()
        }).collect_vec()
      }

      let win_nums = nums_from_str(&win_num_str);
      let have_nums = nums_from_str(&have_num_str);

      (card_num, win_nums, have_nums)
    }).collect_vec()
  }

  pub fn aoc_a() -> i64 {

    let data = get();

    fn score(wins: &Vec<i32>, haves: &Vec<i32>) -> i64 {
      let haves_hs: HashSet<i32> = HashSet::from_iter(haves.clone().into_iter());
      let n = wins.iter().filter(|v| {
        haves_hs.contains(v)
      }).collect_vec().len() as u32;

      if n == 0u32 { 0 } else { i64::pow(2, n-1) }
    }

    data.iter().map(|(_game, wins, haves)| {
      score(wins, haves)
    }).fold(0i64, |acc,v| acc + v as i64)
  }

  pub fn aoc_b() -> i64 {
    let data = get();

    fn succ(game: i32, wins: &Vec<i32>, haves: &Vec<i32>) -> Vec<i32> {
      let haves_hs: HashSet<i32> = HashSet::from_iter(haves.clone().into_iter());
      let n = wins.iter().filter(|v| {
        haves_hs.contains(v)
      }).collect_vec().len() as i32;

      if n == 0i32 { Vec::new() } else { (1..=n).map(|v| game + v).collect_vec() }
    }

    let succ_map: HashMap<i32, Vec<i32>> = data.iter().map(|(game, wins, haves)| {
      (*game, succ(*game, wins, haves))
    }).collect();

    let init_todo: HashMap<i32, i32> = data.iter().map(|v| (v.0, 1)).collect();

    let init : (i64, HashMap<i32, i32>) = (0i64, init_todo);
    data
      .iter()
      .map(|v| v.0)
      .fold(init, |(acc, mut todo), game| {
        let copies = todo.get(&game).map(|v| *v).unwrap_or(0);
        let succs = succ_map.get(&game).unwrap();
        succs.iter().for_each(|s| {
          if todo.contains_key(s) {
            todo.insert(*s, todo.get(s).unwrap() + copies);
          } else {
            todo.insert(*s, copies);
          }
        });
        (acc + copies as i64, todo)
      }).0
  }

}
