

pub mod day_08 {

  use crate::common::common::read_lines;
  use itertools::{Itertools, unfold};
  use std::{str::FromStr, cmp::Ordering};
  use regex::Regex;
  use core::fmt::Formatter;
  use std::collections::{HashSet, HashMap};

  fn parse(path: &str) -> (String, HashMap<String, (String, String)>) {
    let lines = read_lines(path);
    let dirs = lines.get(0).unwrap().clone();

    fn get_r(l: &str, a: usize, b: usize) -> String {
      l.chars().skip(a).take(b-a).collect::<String>()
    }
    //LRV = (NNC, BHD)
    let graph = HashMap::from_iter(
      lines.iter().dropping(2).map(|l| {
        let key = get_r(l, 0, 3);
        let left = get_r(l, 7, 10);
        let right: String = get_r(l, 12, 15);
        (key, (left, right))
      }).into_iter()
    );
    (dirs, graph)
  }

  pub fn aoc_a() -> u64 {
    // parse("test_input/day_08.txt");
    let (dirs, graph) = parse("input/day_08.txt");// parse("test_input/day_08.txt");// 
    let len = dirs.len();
    let dir_chars = dirs.chars().collect_vec();
    unfold((0_usize, 0u64, "AAA"), |(i, steps, key)| {
      let dir = dir_chars.get(*i).unwrap();
      let next = match dir {
        'L' => graph.get(*key).unwrap().0.as_str(),
        'R' => graph.get(*key).unwrap().1.as_str(),
        _ => unimplemented!()
      };
      *i = (*i + 1) % len;
      *steps = *steps + 1;
      *key = next;

      if next == "ZZZ" {
        None
      } else {
        Some(*steps + 1)
      }
    }).last().unwrap()
  }

  fn gcd(mut a: u64, mut b: u64) -> u64 {
    while b > 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
  }

  fn lcd(a: u64, b: u64) -> u64 {
    a * b / gcd(a, b)
  }

  pub fn aoc_b() -> u64 {
    let (dirs, graph) = parse("input/day_08.txt"); //parse("test_input/day_08.txt");
    let starts = graph.keys().filter(|k| k.ends_with("A")).map(|v| v.clone()).collect_vec();
    let dir_chars = dirs.chars().collect_vec();
    let len = dirs.len();
    let steps_per = starts.iter().map(|start| {
      unfold((0_usize, 0u64, start), |(i, steps, key)| {
        let dir = dir_chars.get(*i).unwrap();
        let next = match dir {
          'L' => &graph.get(*key).unwrap().0,
          'R' => &graph.get(*key).unwrap().1,
          _ => unimplemented!()
        };
        *i = (*i + 1) % len;
        *steps = *steps + 1;
        *key = &next;

        if next.ends_with("Z") {
          None
        } else {
          Some(*steps + 1)
        }
      }).last().unwrap()
    }).collect_vec();

    let first = steps_per.get(0).unwrap().clone();
    steps_per.iter().dropping(1).fold(first, |acc, v| {
      lcd(acc, *v)
    })
  }

}