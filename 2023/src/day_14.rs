pub mod day_14 {
  use crate::common::common::read_lines;
  use itertools::{Itertools, unfold};
  use std::{str::FromStr, cmp::Ordering};
  use regex::Regex;
  use core::fmt::Formatter;
  use std::collections::{HashSet, HashMap};

  fn parse(path: &str) -> ((usize, usize), HashMap<(usize, usize), char>) {
    let lines = read_lines(path);
    let jlen = lines.len();
    let ilen = lines.first().unwrap().len();

    let grid: HashMap<(usize, usize), char> = lines.iter().enumerate().flat_map(|(j, l)| {
      l.chars().enumerate().flat_map(|(i, c)| {
        if c == '.' {
          None
        } else {
          Some(((j, i), c))
        }
      }).collect_vec()
    }).collect();

    ((jlen, ilen), grid)
  }

  fn tilt(jlen: usize, ilen: usize, grid: &HashMap<(usize, usize), char>) -> HashMap<(usize, usize), char> {
    let res: HashMap<(usize, usize), char> =(0..ilen).flat_map(|i| {
      (0..jlen).fold((0usize, Vec::<((usize, usize), char)>::new()), |(next_block, mut acc), j| {
        match grid.get(&(j, i)) {
          Some(v) => match v {
            '#' => {
              acc.push(((j, i), '#'));
              (j+1, acc)
            },

            c => {
              acc.push(((next_block, i), *c));
              (next_block+1, acc)
            }
          },
          _ => (next_block, acc)
        }
      }).1
    }).collect();
    res
  }

  fn cycle(jlen: usize, ilen: usize, grid: &HashMap<(usize, usize), char>) -> HashMap<(usize, usize), char> {

    // jseq, iseq, next_block_update, next_coord_update
    // first_seq, second_seq,

    // iterate over all columns first; move toward 0
    enum AlongDim {
      I,
      J
    }
    enum InDir {
      ToZero,
      ToMax
    }

    let cycle = vec![
      (0..jlen, 0..ilen, AlongDim::J, InDir::ToZero), // North
      (0..jlen, 0..ilen, AlongDim::I, InDir::ToZero), // West
      (0..jlen, 0..ilen, AlongDim::J, InDir::ToMax),  // South
      (0..jlen, 0..ilen, AlongDim::I, InDir::ToMax)   // East
    ];

    let cycle_res: HashMap<(usize, usize), char> = cycle.iter().fold(grid.clone(), |grid, (jseq, iseq, along, dir)| {
      let (outer_seq, inner_seq) = match along {
        AlongDim::J => (iseq, jseq),
        AlongDim::I => (jseq, iseq)
      };
      let (jmax, imax) = (jseq.end, iseq.end);
      let init_block = match (along, dir) {
        (AlongDim::J, InDir::ToZero) => 0i32,
        (AlongDim::J, InDir::ToMax) => jmax as i32 - 1,
        (AlongDim::I, InDir::ToZero) => 0i32,
        (AlongDim::I, InDir::ToMax) => imax as i32 - 1
      };
      let outers = outer_seq.start..outer_seq.end;

      let tilted: HashMap<(usize, usize), char> = outers.flat_map(|outer| {
        let inners = inner_seq.start..inner_seq.end;

        inners.fold((init_block, Vec::<((usize, usize), char)>::new()), |(next_block, mut acc), inner| {

          let (ji, ii) = match along {
            AlongDim::J => (inner, outer),
            AlongDim::I => (outer, inner)
          };

          let (j, i) = match (along, dir) {
            (AlongDim::J, InDir::ToZero) => (ji, ii),
            (AlongDim::J, InDir::ToMax) => (jmax - ji - 1, ii),
            (AlongDim::I, InDir::ToZero) => (ji, ii),
            (AlongDim::I, InDir::ToMax) => (ji, imax - ii - 1)
          };

          let key = (j, i);

          match grid.get(&key) {
            Some(v) => match v {
              '#' => {
                acc.push(((j, i), '#'));

                match (along, dir) {
                  (AlongDim::J, InDir::ToZero) => (j as i32 + 1 , acc),
                  (AlongDim::J, InDir::ToMax) => (j as i32 - 1, acc),
                  (AlongDim::I, InDir::ToZero) => (i as i32 + 1, acc),
                  (AlongDim::I, InDir::ToMax) => (i as i32 - 1, acc)
                }
              },
  
              c => {
                match along {
                  AlongDim::J => acc.push(((next_block as usize, i), *c)),
                  AlongDim::I => acc.push(((j, next_block as usize), *c)),
                }
                match dir {
                  InDir::ToZero => (next_block+1, acc),
                  InDir::ToMax => (next_block-1, acc)
                }
              }
            },
            _ => (next_block, acc)
          }
        }).1
      }).collect();
      // (0..jlen).for_each(|j| {
      //   (0..ilen).for_each(|i| {
      //     match tilted.get(&(j, i)) {
      //       Some(c) => print!("{c}"),
      //       _ => print!(".")
      //     }
      //   });
      //   println!();
      // });
      // println!();
      tilted
    });
    cycle_res
  }

  fn score(jlen: usize, grid: &HashMap<(usize, usize), char>) -> u64 {
    grid.keys().map(|k| {
      match grid.get(k).unwrap() {
        'O' => (jlen - k.0) as u64,
        _ => 0
      }
    }).sum()
  }

  fn hash(jlen: usize, ilen: usize, grid: &HashMap<(usize, usize), char>) -> u64 {
    grid.keys().map(|k| {
      match grid.get(k).unwrap() {
        'O' => (jlen - k.0) as u64 * (ilen - k.1) as u64,
        _ => 0
      }
    }).sum()
  }

  pub fn aoc_a() -> u64 {
    let ((jlen, ilen), grid) = parse("test_input/day_14.txt");

    // (0..jlen).for_each(|j| {
    //   (0..ilen).for_each(|i| {
    //     match grid.get(&(j, i)) {
    //       Some(c) => print!("{c}"),
    //       _ => print!(".")
    //     }
    //   });
    //   println!();
    // });
    // println!();

    let tilted = tilt(jlen, ilen, &grid);
    // (0..jlen).for_each(|j| {
    //   (0..ilen).for_each(|i| {
    //     match tilted.get(&(j, i)) {
    //       Some(c) => print!("{c}"),
    //       _ => print!(".")
    //     }
    //   });
    //   println!();
    // });

    score(jlen, &tilted)
  }

  pub fn aoc_b() -> u64 {
    let ((jlen, ilen), grid) = parse("input/day_14.txt");

    println!("INITIAL STATE");
    (0..jlen).for_each(|j| {
      (0..ilen).for_each(|i| {
        match grid.get(&(j, i)) {
          Some(c) => print!("{c}"),
          _ => print!(".")
        }
      });
      println!();
    });
    println!();

    // (1..1000).fold(grid, |acc, i| {
    //   let new_grid = cycle(jlen, ilen, &acc);
    //   let h = hash(jlen, ilen, &new_grid);
    //   let s = score(jlen, &new_grid);
    //   // println!("{i}: hash {h}; score: {s}");
    //   println!("{s}");
    //   new_grid
    // });

    // sequence starts at 142
    let seq: Vec<u64> = vec![
      99848,
      99852,
      99861,
      99859,
      99868,
      99870,
      99861,
      99853,
      99844,
      99862,
      99861,
      99873,
      99862,
      99862,
      99855,
      99849,
      99854,
      99862,
      99875,
      99867,
      99854,
      99856,
      99851,
      99859,
      99854,
      99876,
      99869,
      99859
    ];
    let index = (1_000_000_000 - 141) % 28 - 1 as usize;
    seq.get(index).unwrap().clone()
  }
}
