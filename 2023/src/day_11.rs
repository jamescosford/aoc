pub mod day_11 {
    use std::collections::HashSet;
    use itertools::{Itertools, unfold};
    use crate::common::common::read_lines;

  fn ji(g: &Vec<Vec<char>>, j: usize, i: usize) -> &char {
    g.get(j).unwrap().get(i).unwrap()
  }

  fn parse(path: &str) -> (Vec<usize>, Vec<usize>) {
    let grid = read_lines(path).iter()
      .map(|l| l.chars().collect_vec())
      .collect_vec();
    let ly = grid.len();
    let lx = grid.first().unwrap().len();
    let jis: Vec<(usize, usize)> = (0..ly).cartesian_product(0..lx)
      .fold(Vec::new(), |mut acc, (j, i)| {
        if *ji(&grid, j, i) == '#' {
          acc.push((j, i))
        }
        acc
      });
    (
      jis.iter().map(|v| v.0).collect_vec(),
      jis.iter().map(|v| v.1).collect_vec()
    )
  }

  fn manh_dist(pa: &(i64, i64), pb: &(i64, i64)) -> u64 {
    (i64::abs(pa.0 - pb.0) + i64::abs(pa.1 - pb.1)) as u64
  }

  fn cum_hard(dset: &HashSet<usize>, dlen: usize, scale: usize) -> Vec<usize> {
    unfold((0usize, 0usize), |(offset, j)| {
      if !dset.contains(j) { // has a galaxy
        *offset = *offset + scale - 1;
      }
      let lj = *j;
      *j = *j + 1usize;
      if lj > dlen { None } else { Some(offset.clone()) }
    }).collect_vec()
  }

  fn limpl(scale: usize) -> u64 {
    let (js, is) = parse("input/day_11.txt");

    let jset: HashSet<usize> = js.iter().cloned().collect();
    let iset: HashSet<usize> = is.iter().cloned().collect();

    let row_cum = cum_hard(&jset, js.len(), scale);
    let col_cum = cum_hard(&iset, js.len(), scale);

    let expanded_js = js.iter().map(|j| j + row_cum.get(*j).unwrap()).collect_vec();
    let expanded_is = is.iter().map(|i| i + col_cum.get(*i).unwrap()).collect_vec();

    let i_pairs: HashSet<(usize, usize)> = (0..js.len()).cartesian_product(0..js.len()).flat_map(|(a, b)| {
      if a == b { None } else { Some((usize::min(a, b), usize::max(a, b))) }
    }).collect();

    i_pairs.iter().map(|(a, b)| {
      let pa = (*expanded_js.get(*a).unwrap() as i64, *expanded_is.get(*a).unwrap() as i64);
      let pb = (*expanded_js.get(*b).unwrap() as i64, *expanded_is.get(*b).unwrap() as i64);
      manh_dist(&pa, &pb)
    }).sum()
  }

  pub fn aoc_a() -> u64 {
    limpl(2_usize)
  }

  pub fn aoc_b() -> u64 {
    limpl(1_000_000_usize)
  }
}