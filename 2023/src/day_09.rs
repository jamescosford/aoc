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

  fn parse(path: &str) -> Vec<Vec<i64>> {
    read_lines(path).iter().map(|l| {
      l.split_ascii_whitespace().map(|s| s.parse::<i64>().unwrap()).collect_vec()
    }).collect_vec()
  }

  fn get_diffs(f: &Vec<i64>) -> Vec<Vec<i64>> {
    unfold(f.clone(), |to_diff| {
      let td = to_diff.iter().cloned().collect_vec();
      let mut diffed = diff(&td);
      to_diff.clear();
      to_diff.append(&mut diffed);

      if td.iter().all(|v| *v == 0) {
        None
      } else {        
        Some(td.clone())
      }
    }).collect_vec()
  }

  fn step(a: Vec<i64>) -> Vec<i64> {
    a.iter().rev().fold((Vec::<i64>::new(), 0i64), |(acc_vec, acc_sum), v| {
      let mut to_alter = acc_vec.clone();
      let new_sum = acc_sum + *v;
      to_alter.push(new_sum);
      (to_alter.clone(), new_sum)
    }).0
  }

  fn step_func_from_diffs(ds: &Vec<Vec<i64>>) -> Vec<i64> {
    ds.iter().map(|v| v.last().unwrap().clone()).collect_vec()
  }

  pub fn aoc_a() -> u64 {
    let a: Vec<i64> = vec![0, 3, 6, 9, 12, 15];
    let b: Vec<i64> = vec![1, 3, 6, 10, 15, 21];
    let c: Vec<i64> = vec![10, 13, 16, 21, 30, 45];

    let diffs = get_diffs(&a);

    println!("DIFFS");
    diffs.iter().for_each(|d| {
      d.iter().for_each(|v| print!("{v} "));
      println!();
    });

    let step_func = step_func_from_diffs(&diffs);

    println!("STEP_FUNCT");
    step_func.iter().for_each(|v| print!("{v} "));
    println!("END STEP FUNCT");

    step(vec![2, 6, 15]).iter().for_each(|v| println!("{v}"));

    unfold(step_func, |f| {
      let to_step = f.clone();
      let mut next = step(to_step);
      f.clear();
      f.append(&mut next);
      next.iter().for_each(|v| print!("{v} "));
      println!();
      Some(f.clone())
    }).take(10).for_each(|v| print!("{} ", v.last().unwrap()));

    1u64
  }

  pub fn aoc_b() -> u64 {
    1u64
  }
}