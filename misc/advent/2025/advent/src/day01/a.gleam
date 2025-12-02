import gleam/string
import gleam/int
import gleam/list
import gleam/result
import simplifile

pub fn mod(n, mod) -> Int {
    case n >= 0 {
        True -> n % mod
        False -> {mod + {n % mod}} % mod
    }
}

pub fn parse(input) {
    string.trim(input) |>
        string.split(on: "\n") |>
        list.map(fn(line) { 
            let assert [first, ..rest] = string.to_graphemes(line)
            case first {
                "L" -> -1
                "R" -> 1
                _ -> panic as "impossible"
            } * {string.join(rest, "") |> int.parse |> result.unwrap(0)}
        })
}

pub fn run(input) {
    parse(input) |>
        list.scan(50, fn(n, t) {
            mod(n + t, 100)
        }) |>
        list.filter(fn(x) { x == 0 }) |>
        list.length
}

pub fn n_passes(n, t, m) -> Int {
    // we can't negate in a guard clause (gross?)
    let nt = int.negate(t)
    case t {
        // if we're turning right, ⌊(n+t)/m⌋
        _ if t > 0 -> {n + t} / m
        // if we're turning left <1 zero point, 0
        _ if n > nt -> 0
        // if we're at zero, do not pass go, return ⌊t/m⌋
        _ if n == 0 -> int.absolute_value(t/m)
        // if we're turning left >1 zero point, 1 + ⌊(n+t)/m⌋
        _ -> 1 + int.absolute_value(n + t) / m
    }
}

pub fn run2(input) {
    let #(_, n) = parse(input) |>
        list.scan(#(50, 0), fn(state, t) {
            let #(n, passes) = state
            #(mod(n+t, 100), passes + n_passes(n, t, 100))
        }) |>
        list.last() |>
        result.unwrap(#(0,0))
    n
}

pub fn main() {
    let assert Ok(sample) = simplifile.read(from: "./src/day01/sample.txt")
    let assert Ok(content) = simplifile.read(from: "./src/day01/input.txt")

    assert run(sample) == 3
    echo #("part a", run(content))

    assert run2(sample) == 6
    echo #("part b", run2(content))
}
