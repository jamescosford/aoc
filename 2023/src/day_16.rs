
use std::collections::{ HashSet, HashMap };
use itertools::{unfold, Itertools};
use crate::common::common::read_lines;

fn parse(path: &str) -> (usize, usize, HashMap<(usize, usize), char>) {
  let lines = read_lines(path);
  let lj = lines.len();
  let li = lines.first().unwrap().len();
  let grid: HashMap<(usize, usize), char> = lines.iter().enumerate().flat_map(|(j, l)| {
    l.chars().enumerate().flat_map(move |(i, c)| {
      if c == '.' { None } else { Some(((j, i), c)) }
    })
  }).collect();

  (lj, li, grid)
}

fn show_grid(lj: &usize, li: &usize, grid: &HashMap<(usize, usize), char>) -> () {
  (0..*lj).for_each(|j| {
    (0..*li).for_each(|i| {
      match grid.get(&(j, i)) {
        Some(c) => print!("{c}"),
        None => print!(".")
      };
    });
    println!();
  })
}
fn show_acc(lj: &usize, li: &usize, grid: &HashMap<(usize, usize), HashSet<char>>) -> () {
  (0..*lj).for_each(|j| {
    (0..*li).for_each(|i| {
      match grid.get(&(j, i)) {
        Some(c) => {
          if c.len() == 1 {
            print!("{}", c.iter().next().unwrap());
          } else {
            print!("{}", c.len());
          }
        },
        None => print!(".")
      };
    });
    println!();
  })
}
fn score_acc(grid: &HashMap<(usize, usize), HashSet<char>>) -> u64 {
  grid.keys().len() as u64
}

fn down(max: &(usize, usize), p: &(usize, usize)) -> Option<(usize, usize)> {
  let p_new = (p.0 + 1, p.1);
  if p_new.0 > max.0 - 1 { None } else { Some(p_new) }
}

fn up(_max: &(usize, usize), p: &(usize, usize)) -> Option<(usize, usize)> {
  let p_new = (p.0 as i64 - 1, p.1);
  if p_new.0 < 0 { None } else { Some((p.0 - 1, p.1)) }
}

fn left(_max: &(usize, usize), p: &(usize, usize)) -> Option<(usize, usize)> {
  let p_new = (p.0, p.1 as i64 - 1);
  if p_new.1 < 0 { None } else { Some((p.0, p.1-1)) }
}

fn right(max: &(usize, usize), p: &(usize, usize)) -> Option<(usize, usize)> {
  let p_new = (p.0, p.1 + 1);
  if p_new.1 > max.1 - 1 { None } else { Some(p_new) }
}

fn prop_grid(lj: &usize, li: &usize, grid: &HashMap<(usize, usize), char>, start: ((usize, usize), char)) -> HashMap<(usize, usize), HashSet<char>> {
  let mut acc: HashMap<(usize, usize), HashSet<char>> = HashMap::new();
  let active: Vec<((usize, usize), char)> = vec![start];
  let max = (*lj, *li);

  unfold((&mut acc, active), |(acc, active)| {
    // println!("active len: {}", active.len());
    match active.pop() {
      None => None,
      Some(a) => {

        let cont = match acc.get(&a.0) {
          None => true,
          Some(c) if c.contains(&a.1) => false,
          _ => true
        };

        if !cont {
          Some(())
        } else {
          // update overall accumulator 
          match acc.get_mut(&a.0) {
            None => {
              acc.insert(a.0, HashSet::from([a.1]));
            },
            Some(vc) => {
              vc.insert(a.1);
            }
          };

          // step simulation
          // println!("({} {}) {}", a.0.0, a.0.1, a.1);
          let (p, c) = a;
          match c {
            '>' => {
              match grid.get(&p) {
                None => active.extend(vec![right(&max, &p).map(|new_p| (new_p, '>'))].iter().flatten()),
                Some(v) => {
                  match *v {
                    '/' => active.extend(vec![up(&max, &p).map(|new_p| (new_p, '^'))].iter().flatten()),
                    '\\' => active.extend(vec![down(&max, &p).map(|new_p| (new_p, 'v'))].iter().flatten()),
                    '-' => active.extend(vec![right(&max, &p).map(|new_p| (new_p, '>'))].iter().flatten()),
                    '|' => {
                      active.extend(vec![
                        up(&max, &p).map(|new_p| (new_p, '^')),
                        down(&max, &p).map(|new_p| (new_p, 'v')),
                      ].iter().flatten());
                    },
                    _ => unimplemented!()
                  };
                }
              }
            },
            'v' => {
              match grid.get(&p) {
                None => active.extend(vec![down(&max, &p).map(|new_p| (new_p, 'v'))].iter().flatten()),
                Some(v) => {
                  match *v {
                    '/' => active.extend(vec![left(&max, &p).map(|new_p| (new_p, '<'))].iter().flatten()),
                    '\\' => active.extend(vec![right(&max, &p).map(|new_p| (new_p, '>'))].iter().flatten()),
                    '-' => active.extend(vec![
                      left(&max, &p).map(|new_p| (new_p, '<')),
                      right(&max, &p).map(|new_p| (new_p, '>'))
                    ].iter().flatten()),
                    '|' => active.extend(vec![down(&max, &p).map(|new_p| (new_p, 'v'))].iter().flatten()),
                    _ => unimplemented!()
                  };
                }
              }
            },
            '<' => {
              match grid.get(&p) {
                None => active.extend(vec![left(&max, &p).map(|new_p| (new_p, '<'))].iter().flatten()),
                Some(v) => {
                  match *v {
                    '/' => active.extend(vec![down(&max, &p).map(|new_p| (new_p, 'v'))].iter().flatten()),
                    '\\' => active.extend(vec![up(&max, &p).map(|new_p| (new_p, '^'))].iter().flatten()),
                    '-' => active.extend(vec![left(&max, &p).map(|new_p| (new_p, '<'))].iter().flatten()),
                    '|' => active.extend(vec![
                        up(&max, &p).map(|new_p| (new_p, '^')),
                        down(&max, &p).map(|new_p| (new_p, 'v'))
                      ].iter().flatten()),
                    _ => unimplemented!()
                  };
                }
              }
            },
            '^' => {
              match grid.get(&p) {
                None => active.extend(vec![up(&max, &p).map(|new_p| (new_p, '^'))].iter().flatten()),
                Some(v) => {
                  match *v {
                    '/' => active.extend(vec![right(&max, &p).map(|new_p| (new_p, '>'))].iter().flatten()),
                    '\\' => active.extend(vec![left(&max, &p).map(|new_p| (new_p, '<'))].iter().flatten()),
                    '-' => active.extend(vec![
                        left(&max, &p).map(|new_p| (new_p, '<')),
                        right(&max, &p).map(|new_p| (new_p, '>'))
                        ].iter().flatten()),
                    '|' => active.extend(vec![up(&max, &p).map(|new_p| (new_p, '^'))].iter().flatten()),
                    _ => unimplemented!()
                  };
                }
              }
            },
            _ => unimplemented!()
          }
          // active.iter().for_each(|v| print!("({} {}) {} ; ", v.0.0, v.0.1, v.1));
          // println!();
          Some(())
        }
      }
    }
  }).last();
  acc
}

pub fn aoc_a() -> u64 {
  let (lj, li, grid) = parse("input/day_16.txt");
  show_grid(&lj, &li, &grid);

  let acc = prop_grid(&lj, &li, &grid, ((0,0), '>'));
  show_acc(&lj, &li, &acc);

  score_acc(&acc)
}

fn run_with(start: ((usize, usize), char), lj: &usize, li: &usize, grid: &HashMap<(usize, usize), char>) -> u64 {
  let acc = prop_grid(&lj, &li, &grid, start);
  score_acc(&acc)
}

pub fn aoc_b() -> u64 {
  let (lj, li, grid) = parse("input/day_16.txt");
  vec![
    (0..li).map(|i| ((0, i), 'v')).map(|start| run_with(start, &lj, &li, &grid)).collect_vec(),    // top 
    (0..li).map(|i| ((lj-1, i), '^')).map(|start| run_with(start, &lj, &li, &grid)).collect_vec(), // bottom
    (0..lj).map(|j| ((j, 0), '>')).map(|start| run_with(start, &lj, &li, &grid)).collect_vec(), // left
    (0..lj).map(|j| ((j, li-1), '<')).map(|start| run_with(start, &lj, &li, &grid)).collect_vec(), // right
  ].iter().flatten().max().unwrap().clone()
}
