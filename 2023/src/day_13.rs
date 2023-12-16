
use std::collections::HashSet;
use itertools::{Itertools, unfold};
use crate::common::common::read_lines;

fn parse(path: &str) -> Vec<Vec<Vec<char>>> {
  let lines = read_lines(path);

  let init: Vec<Vec<Vec<char>>> = vec![Vec::<Vec<char>>::new()];
  lines.iter().fold(init, |mut acc, l| {
    if l.is_empty() {
      acc.push(Vec::<Vec<char>>::new());
      acc
    } else {
      acc.last_mut().unwrap().push(l.chars().collect_vec());
      acc
    }
  })
}

fn print_grid(g: &Vec<Vec<char>>) -> () {
  g.iter().for_each(|r| {
    r.iter().for_each(|c| {
      print!("{c}");
    });
    println!();
  });
}

// reduce rows to a single value, the product of all i values with a #
fn hash_horz(g: &Vec<Vec<char>>) -> Vec<u64> {
  g.iter().map(|r| {
    r.iter().enumerate().flat_map(|(i, v)| {
      if *v == '#' { Some((i + 1) as u64) } else { None }
    }).sum()
  }).collect_vec()
}

fn ji(g: &Vec<Vec<char>>, j: &usize, i: &usize) -> char { 
  *g.get(*j).unwrap().get(*i).unwrap()
}

fn hash_vert(g: &Vec<Vec<char>>) -> Vec<u64> {
  let is = 0..g.get(0).unwrap().len();
  is.map(|i| {
    let js = 0..g.len();
    js.flat_map(|j| {
      let c = ji(g, &j, &i);
      if c == '#' { Some((j + 1) as u64) } else { None }
    }).sum()
  }).collect_vec()
}

fn reflect_vert(g: &Vec<Vec<char>>) -> Option<usize> {
  let hash = hash_horz(g);
  let pairs = (1..hash.len()).flat_map(|j| {
    if hash.get(j)? == hash.get(j-1)? {
      Some((j-1, j))
    } else { None }
  }).collect_vec();
  let to_checks = pairs.iter().map(|j| {
    unfold(vec![j.clone()], |acc| {
      let (la, lb) = acc.last().unwrap();
      let (a, b) = (*la - 1, *lb + 1);
      if a < 0 || b >=hash.len() {
        None
      } else {
        acc.push((a, b));
        Some((a, b))
      }
    }).collect_vec()
  }).collect_vec();

}

pub fn aoc_a() -> u64 {
  let grids = parse("test_input/day_13.txt");

  grids.iter().for_each(|g| { print_grid(g); println!() });

  grids.iter().for_each(|g| {
    hash_horz(g).iter().for_each(|h| println!("hh {h}"));
    println!();
    hash_vert(g).iter().for_each(|h| println!("hv {h}"));
    println!();println!();
  });

  1u64
}

pub fn aoc_b() -> u64 {
  1u64
}
