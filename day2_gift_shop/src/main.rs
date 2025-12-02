use std::fs;

#[derive(Debug,Clone)]
struct Range {
    from: u64,
    to: u64
}

fn parse(input: String) -> Vec<Range> {
    let ranges: Vec<Range> = input
        .trim()
        .split(',')
        .filter_map(|segment| {
            let (start_str, end_str) = segment.split_once('-')?;

            Some(Range {
                from: start_str.parse::<u64>().ok()?,
                to: end_str.parse::<u64>().ok()?,
            })
        })
        .collect();
    return ranges;
}

fn next_power_of_10(n: u64) -> u64 {
    if n == 0 { return 10; }
    let digits = n.ilog10() + 1;
    10u64.pow(digits)
}

fn split_by_magnitude(range: Range) -> Vec<Range> {
    let mut result = Vec::new();
    let mut current = range.from;

    while current <= range.to {
        let boundary: u64 = next_power_of_10(current);

        if boundary > range.to {
            result.push(Range {from: current, to: range.to});
            break;
        }
        result.push(Range{from: current, to: boundary - 1});
        current = boundary;
    }

    result
}

fn part1(input: String) -> u64 {
    let ranges: Vec<Range> = parse(input);
    let mut found: Vec<u64> = Vec::new();
    for range in ranges {
        let real_ranges: Vec<Range> = split_by_magnitude(range);
        for real_range in real_ranges {
            let digits = real_range.from.ilog10() + 1;
            if digits % 2 != 0 {
                continue;
            }
            let k = digits / 2;
            let multiplier = 10u64.pow(k) + 1;

            let x_min: u64 = (real_range.from + multiplier - 1) / multiplier;
            let x_max: u64 = real_range.to / multiplier;

            for x in x_min..=x_max {
                let candidate: u64 = x * multiplier;
                if candidate >= real_range.from && candidate <= real_range.to {
                    found.push(candidate);
                }
            }
        }
    }
    found.iter().sum()
} 

fn main() {
    let input_raw = fs::read_to_string("input.txt").expect("could not read");
    let result: u64 = part1(input_raw);
    println!("{}", result);    
}
