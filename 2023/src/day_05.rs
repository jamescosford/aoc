pub mod day_05 {
  use crate::common::common::read_lines;
  use itertools::Itertools;
  use regex::Regex;
  use std::collections::{HashSet, HashMap};
  use std::ops::Range;
  use std::str::FromStr;
  use core::fmt::Formatter;
  use std::cmp::{max, min};
  use itertools::unfold;

  struct RangeMapper {
    from: String,
    to: String,
    fs: Vec<Box<dyn Fn(u64) -> Option<u64>>>,
    rs: Vec<Box<dyn Fn(&Range<u64>) -> Option<Range<u64>>>>
  }

  fn to_rm(ss: &Vec<&String>) -> RangeMapper {
    let mut iter = ss.iter();
    let (from, to) = iter.next().map(|v| {
      let parts = v.split(" ").collect_vec();
      parts.get(0).map(|v| {
        let parts = v.split("-to-").collect_vec();
        (String::from(*parts.get(0).unwrap()), String::from(*parts.get(1).unwrap()))
      }).unwrap()
    }).unwrap();
    let collected = iter.cloned().collect_vec();
    let fs = get_fs(&collected);
    let rs = get_rs(&collected);
    RangeMapper { from: from, to: to, fs: fs, rs: rs }
  }

  fn get_fs(ss: &Vec<&String>) -> Vec<Box<dyn Fn(u64) -> Option<u64>>> {
    ss.iter().map(|s| {
      let parts = s.split(" ").flat_map(|s| {
        s.parse::<u64>()
      }).collect_vec();
      let (dest_start, src_start, len) = (
        parts.get(0).unwrap(),
        parts.get(1).unwrap(),
        parts.get(2).unwrap()
      );
      let (ss, ds, ln) = (*src_start, *dest_start, *len);
      
      let x: Box<dyn Fn(u64) -> Option<u64>> = Box::new(move |nn| {
        if nn >= ss && nn < ss + ln {
          Some(nn - ss + ds)
        } else {
          None
        }
      });
      x
    }).collect_vec()
  }

  fn get_rs(ss: &Vec<&String>) -> Vec<Box<dyn Fn(&Range<u64>) -> Option<Range<u64>>>> {
    ss.iter().map(|s| {
      let parts = s.split(" ").flat_map(|s| {
        s.parse::<u64>()
      }).collect_vec();
      let (dest_start, src_start, len) = (
        parts.get(0).unwrap(),
        parts.get(1).unwrap(),
        parts.get(2).unwrap()
      );
      let (ss, ds, ln) = (*src_start, *dest_start, *len);
      
      let x: Box<dyn Fn(&Range<u64>) -> Option<Range<u64>>> = Box::new(move |nn| {

        let lower = if nn.start < ss { nn.clone() } else { ss..ss+ln };
        let upper = if nn.start > ss { nn.clone() } else { ss..ss+ln };

        let overlap = lower.end > upper.start;
        if overlap {
          let new_start = upper.start;
          let new_end = min(lower.end, upper.end);

          let dest_start = ds + (new_start - ss);
          let dest_end = dest_start + new_end - new_start;
          Some(dest_start..dest_end)
        } else { None }
      });
      x
    }).collect_vec()
  }

  fn get() -> (Vec<u64>, Vec<RangeMapper>) {
    let lines = read_lines("input/day_05.txt");

    let mut liter = lines.iter();

    let seeds = liter.next().unwrap().split(":").collect_vec().get(1).unwrap().trim().split(" ").flat_map(|v| {
      v.parse::<u64>()
    }).collect_vec();

    liter = liter.dropping(1);

    let rms = liter.clone().batching(|it| {
      let x = it.take_while(|v| v.len() != 0).collect_vec();
      if x.len() > 0 {
        Some(x)
      } else {
        None
      }
    }).map(|v| {
      to_rm(&v)
    }).collect_vec();

    (seeds, rms)
  }

  pub fn aoc_a() -> u64 {
    let (seeds, rms) = get();

    fn pw_eval(v: u64, fs: &Vec<Box<dyn Fn(u64) -> Option<u64>>>) -> u64 {
      fs.iter().flat_map(|f| f(v)).next().unwrap_or(v)
    }

    seeds.iter().map(|seed| {
      rms.iter().fold(*seed, |acc, rm| {
        pw_eval(acc, &rm.fs)
      })
    }).min().unwrap()
  }

  pub fn aoc_b() -> u64 {

    let (seeds, rms) = get();
    let seed_rs = seeds.chunks(2).map(|v| {
      v[0]..v[0]+v[1]
    }).collect_vec();

    fn pw_eval(v: &Range<u64>, fs: &Vec<Box<dyn Fn(&Range<u64>) -> Option<Range<u64>>>>) -> Vec<Range<u64>> {
      let rs = fs.iter().flat_map(|f| f(v)).collect_vec();
      if rs.is_empty() { vec![v.clone()] } else { rs }
    }

    rms.iter().fold(seed_rs, |acc, rm| {
      acc.iter().flat_map(|v| pw_eval(v, &rm.rs)).collect_vec()
    }).iter().map(|v| v.start).min().unwrap()
  }

}
