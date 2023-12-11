pub mod day_09 {
  use crate::common::common::read_lines;
  use itertools::{Itertools, unfold};
  use std::{str::FromStr, cmp::Ordering};
  use regex::Regex;
  use core::fmt::Formatter;
  use std::collections::{HashSet, HashMap};


  fn diff(f: &Vec<i64>) -> Vec<i64> {
    let l = f.len();
    (1..l).map(|i| {
      let a = f.get(i - 1).unwrap();
      let b = f.get(i).unwrap();
      *b-*a
    }).collect_vec()
  }

  fn get_gen(f: &Vec<i64>) -> Vec<i64> {
     
  }

  pub fn aoc_a() -> u64 {
    1u64
  }

  pub fn aoc_b() -> u64 {
    1u64
  }
}