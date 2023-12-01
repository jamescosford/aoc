pub mod common {
    use std::{
        fs::File,
        io::{self, BufRead},
        path::Path,
        str::FromStr,
    };

    use regex::Regex;

    pub fn read_lines<P>(path: P) -> Vec<String>
    where
        P: AsRef<Path>,
    {
        io::BufReader::new(File::open(path).unwrap())
            .lines()
            .into_iter()
            .map(|l| l.unwrap())
            .collect()
    }

    fn chunk<A>(to_chunk: &Vec<A>, len: usize) -> Vec<Vec<&A>> {
        to_chunk
            .iter()
            .fold((Vec::new(), Vec::new()), |acc, i| {
                let (mut res, mut cur) = acc;
                cur.push(i);
                if cur.len() == len {
                    res.push(cur);
                    (res, Vec::new())
                } else {
                    (res, cur)
                }
            })
            .0
    }

    pub fn single<'a, T: FromStr>(re: Regex, s: &str) -> Result<T, &'a str> {
        let caps = re.captures(s).ok_or("No caps")?;
        let cap = caps.get(1).ok_or("Missing cap")?;
        cap.as_str().parse::<T>().map_err(|_| "Parse failed")
    }
}
