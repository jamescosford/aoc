pub mod day_06 {

  use std::ops::Range;

  // Time:        48     98     90     83
  // Distance:   390   1103   1112   1360

  fn to_win_range(t: &i64, r: &i64) -> Range<i64> {
    let (a, b, c) = (-1 as f64, *t as f64, -(*r) as f64);
    let sqrted = f64::sqrt(b*b - 4f64 * a * c);
    let r0 = ((-b + sqrted) / -2f64) as i64;
    let r1 = ((-b - sqrted) / -2f64) as i64;
    r0..r1
  }

  pub fn aoc_a() -> u64 {
    let races = vec![(48, 390), (98, 1103), (90, 1112), (83, 1360)];
    races.iter()
      .map(|(t, r)| to_win_range(t, r))
      .map(|r| {
        (r.end - r.start) as u64
      })
      .fold(1u64, |acc,v| acc*v)
  }

  pub fn aoc_b() -> u64 {
    let (t, r) = (48989083_i64, 390110311121360_i64);
    let r = to_win_range(&t, &r);
    (r.end - r.start) as u64
  }
}
