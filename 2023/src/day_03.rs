pub mod day_03 {
  use crate::common::common::read_lines;
  use itertools::Itertools;
  use regex::Regex;
  use std::collections::{HashSet, HashMap};
  use std::ops::Range;
  use std::str::FromStr;
  use core::fmt::Formatter;
  use std::cmp::max;

  #[derive(Hash, Eq, PartialEq, Debug)]
  struct Pt {
    i: usize,
    j: usize, 
  }
  impl std::fmt::Display for Pt {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
      write!(f, "{}, {}", self.i, self.j)
    }
  }

  #[derive(Hash, Eq, PartialEq, Debug)]
  struct Num {
    i: usize,
    r: Range<usize>,
    v: i32
  }
  impl std::fmt::Display for Num {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
      write!(f, "{}, {}-{}, {}", self.i, self.r.start, self.r.end, self.v)
    }
  }

  fn get(re_sym: Regex) -> (Vec<Num>, Vec<Pt>) {
    let re_num = Regex::from_str(r"(\d+)").unwrap();

    let lines = read_lines("input/day_03.txt");

    let nums = lines
      .iter()
      .enumerate()
      .flat_map(|(i, l)| {
        re_num.captures_iter(l).flat_map(|v| {
          v.iter().next().map(|c| {
            match c {
              Some(m) =>
                Some(Num {i: i, r: (m.start() .. m.end()), v: m.as_str().parse::<i32>().unwrap()}),
              None => None
            }
          }).flatten()
        }).collect_vec()
      }).collect_vec();

    let syms = lines.iter().enumerate().flat_map(|(i, l)| {
      re_sym.captures_iter(l).flat_map(|v| {
        v.iter().next().map(|c| {
          match c {
            Some(m) => Some( Pt { i: i, j: m.start() }),
            None => None
          }
        }).flatten()
      }).collect_vec()
    }).collect_vec();

    (nums, syms)
  }

  fn decide_num(num: &Num, sym_hs: &HashSet<Pt>) -> bool {
    let Num { i, r, v: _ } = num;
    let rstart = max((r.start as i64) - 1, 0) as usize;

    (rstart .. (r.end) + 1).flat_map(|x| {
      let istart = max((*i as i64) - 1, 0) as usize;
      (istart ..= i + 1).map(move |y| {
        Pt { i: y, j: x }
      })
    }).any(|p| sym_hs.contains(&p))
  }

  pub fn aoc_a() -> i64 {
    let (nums, syms) = get(Regex::from_str(r"[^0-9.]").unwrap()); 
    let sym_hs: HashSet<Pt> = syms.into_iter().collect();

    nums.iter().flat_map(|n| { 
      match decide_num(n, &sym_hs) {
        true => Some(n.v),
        _ => None
      }
    }).fold(0i64, |acc, v| acc + v as i64)
  }

  fn near_star(num: &Num, sym_hs: &HashSet<Pt>) -> Vec<(Pt, i32)> {
    let Num { i, r, v } = num;
    let rstart = max((r.start as i64) - 1, 0) as usize;

    (rstart .. (r.end) + 1).flat_map(|x| {
      let istart = max((*i as i64) - 1, 0) as usize;
      (istart ..= i + 1).map(move |y| {
        Pt { i: y, j: x }
      })
    }).flat_map(|p| {
      match sym_hs.contains(&p) {
        true => Some((Pt { i: p.i, j: p.j}, *v)),
        _ => None
      }
    }).collect_vec()
  }

  pub fn aoc_b() -> i64 {
    let (nums, syms) = get(Regex::from_str(r"\*").unwrap());

    let sym_hs: HashSet<Pt> = syms.into_iter().collect();
    let near_stars = nums.iter().flat_map(|n| near_star(n, &sym_hs)).collect_vec();
    
    let correlated: HashMap<Pt, (i32, i64)> = near_stars.iter().fold(HashMap::new(), |mut acc, v| {
      let k = Pt { i: v.0.i, j: v.0.j };
      if acc.contains_key(&v.0) {
        let (count, product) = acc.get(&v.0).unwrap();
        acc.insert(k, (count + 1, *product * v.1 as i64));
        acc
      } else {
        acc.insert(k, (1, v.1 as i64));
        acc
      }
    });

    correlated.values().fold(0i64, |acc, (count, product)| {
      if *count == 2i32 {
        acc + product
      } else {
        acc
      }
    })
  }
}
