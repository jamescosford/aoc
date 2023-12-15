pub mod day_15 {

  use crate::common::common::read_lines;
  use itertools::{Itertools, unfold};
  use std::{str::FromStr, cmp::Ordering};
  use regex::Regex;
  use core::fmt::Formatter;
  use std::collections::{HashSet, HashMap};

  fn hash(s: &str) -> u64 {
    s.chars().fold(0u64, |acc, c| {
      ((acc + c as u64) * 17) % 256u64
    })
  }

  pub fn aoc_a() -> u64 {
    read_lines("input/day_15.txt").first().unwrap().split(",").map(|s| {
      hash(s)
    }).sum()
  }

  pub fn aoc_b() -> u64 {
    let mut r: Vec<Vec<(String, u64)>> = (0..256).map(|_| {
      Vec::<(String, u64)>::new()
    }).collect_vec();

    read_lines("input/day_15.txt").first().unwrap().split(",").for_each(|s| {
      let chars = s.chars().collect_vec();
      if *chars.last().unwrap() == '-' {
        let id: String = (0..chars.len()-1).map(|i| chars.get(i).unwrap()).collect();
        let _box = hash(&id) as usize;
        let box_to_update = r.get_mut(_box).unwrap();

        let maybe_pos = box_to_update.iter().enumerate().fold(None, |acc, (i, v)| {
          match acc {
            None => {
              if v.0 == id { Some(i) } else { None }
            },
            Some(_) => acc
          }
        });

        match maybe_pos {
          Some(i) => {
            box_to_update.remove(i);
          },
          None => ()
        }

      } else {
        let bits = s.split("=").collect_vec();
        let id = String::from_str(bits.get(0).unwrap()).unwrap();
        let _box = hash(&id) as usize;
        // println!("B _box = {_box} {id}");
        let fl = bits.get(1).unwrap().parse::<u64>().unwrap();
        let box_to_update = r.get_mut(_box).unwrap();

        let maybe_pos = box_to_update.iter().enumerate().fold(None, |acc, (i, v)| {
          match acc {
            None => {
              if v.0 == id { Some(i) } else { None }
            },
            Some(_) => acc
          }
        });

        match maybe_pos {
          // append
          None => box_to_update.push((id, fl)),
          Some(i) => {
            box_to_update.insert(i, (id, fl));
            box_to_update.remove(i+1);
          }
        }
      }

      // (0..256).for_each(|i| {
      //   let to_print = r.get(i).unwrap();
      //   if !to_print.is_empty() {
      //     print!("Box {i} ");
      //     to_print.iter().for_each(|v| {
      //       print!("[{} {}] ", v.0, v.1)
      //     });
      //     println!();
      //   }
      // });
      // println!();
    });

    (0..256).flat_map(|i| {
      let box_to_score = r.get(i).unwrap();
      let maybe_scope: Option<u64> = if box_to_score.is_empty() {
        None
      } else {
        Some(box_to_score.iter().enumerate().map(|(j, v)| {
          (i as u64 + 1) * (j as u64 + 1) * v.1
        }).sum())
      };
      maybe_scope
    }).sum()

  }
}