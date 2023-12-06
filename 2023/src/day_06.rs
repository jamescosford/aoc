pub mod day_06 {

  use itertools::Itertools;

  pub fn aoc_a() -> u64 {

    // Time:        48     98     90     83
    // Distance:   390   1103   1112   1360

    let races = vec![(48, 390), (98, 1103), (90, 1112), (83, 1360)];

    let to_win_range = |t:&i32, r:&i32| {
      let (a, b, c) = (-1 as f64, *t as f64, -(*r) as f64);

      let to_sqrt = (b*b - 4f64 * a * c) as f64;
      let sqrted = f64::sqrt(to_sqrt);

      let dist = |v0:i32| -1*i32::pow(v0, 2) + t*v0 - r;

      let r0 = ((-b + sqrted) / -2f64) as i32;
      let r1 = ((-b - sqrted) / -2f64) as i32;
      r0..r1
    };

    races.iter()
      .map(|(t, r)| to_win_range(t, r))
      .map(|r| r.len() as u64)
      .fold(1u64, |acc,v| acc*v)
  }

  pub fn aoc_b() -> u64 {
    let (t, r) = (48989083_i64, 390110311121360_i64);
    // Distance:   

    let to_win_range = |t:&i64, r:&i64| {
      let (a, b, c) = (-1 as f64, *t as f64, -(*r) as f64);

      let to_sqrt = (b*b - 4f64 * a * c) as f64;
      let sqrted = f64::sqrt(to_sqrt);

      let dist = |v0:i64| -1*i64::pow(v0, 2) + t*v0 - r;

      let r0 = ((-b + sqrted) / -2f64) as i64;
      let r1 = ((-b - sqrted) / -2f64) as i64;
      r0..r1
    };

    let r = to_win_range(&t, &r);
    (r.end - r.start) as u64
  }

  /*

      let (t, r) = (7, 9);

    let dist = |v0:i32| -1*i32::pow(v0, 2) + t*v0 - r;
      (0..15).map(|v| (v, dist(v))).for_each(|v| print!("({}, {}) ", v.0, v.1));
    println!("");

    let (a, b, c) = (-1, t, -r);

    let (r0, r1) = {
      let to_sqrt = (b*b - 4 * a * c) as f64;
      let sqrted = f64::sqrt(to_sqrt) as i32;
      (
        (-b + sqrted) / -2,
        (-b - sqrted) / -2
      )
    };

    println!("r0: {r0} r1: {r1}"); */

}
