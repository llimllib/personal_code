import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub type Range {
  Range(start: Int, end: Int)
}

pub fn parse(input) {
  string.trim(input)
  |> string.split(on: ",")
  |> list.map(fn(range) {
    let ends = string.split(range, on: "-")
    let start =
      list.first(ends) |> result.unwrap("0") |> int.parse |> result.unwrap(0)
    let end =
      list.last(ends) |> result.unwrap("0") |> int.parse |> result.unwrap(0)
    Range(start, end)
  })
}

pub fn dubs(range: Range) {
  let start = int.to_string(range.start)
  let front =
    string.slice(start, 0, string.length(start) / 2)
    |> int.parse
    |> result.unwrap(0)
  acc_dubs(front, range, [])
}

pub fn acc_dubs(front: Int, range: Range, acc: List(Int)) {
  let dub =
    string.append(int.to_string(front), int.to_string(front))
    |> int.parse
    |> result.unwrap(0)
  case dub >= range.start, dub <= range.end {
    True, True -> acc_dubs(front + 1, range, [dub, ..acc])
    True, False -> acc
    False, True -> acc_dubs(front + 1, range, acc)
    False, False -> panic as "impossible"
  }
}

pub fn part_a(input) {
  parse(input)
  |> list.map(dubs)
  |> list.flatten
  |> list.fold(0, fn(a, b) { a + b })
}

pub fn atoi(n) {
  int.parse(n) |> result.unwrap(0)
}

pub fn is_sym(n) {
  let ns = n |> int.to_string
  let l = ns |> string.length
  list.any(
    [
      atoi(string.repeat(string.slice(ns, 0, 1), int.max(l, 2))),
      atoi(string.repeat(string.slice(ns, 0, 2), int.max(l / 2, 2))),
      atoi(string.repeat(string.slice(ns, 0, 3), int.max(l / 3, 2))),
      atoi(string.repeat(string.slice(ns, 0, 4), int.max(l / 4, 2))),
      atoi(string.repeat(string.slice(ns, 0, 5), int.max(l / 5, 2))),
    ],
    fn(x) { x == n && n > 9 },
  )
}

pub fn part_b(input) {
  let ranges = parse(input)
  list.fold(
    list.flat_map(ranges, fn(r) { acc_nubs(r.start, r.end, []) }),
    0,
    int.add,
  )
}

pub fn acc_nubs(start, end, acc) {
  case start > end, is_sym(start) {
    True, _ -> acc
    _, True -> acc_nubs(start + 1, end, [start, ..acc])
    _, False -> acc_nubs(start + 1, end, acc)
  }
}

pub fn main() {
  let assert Ok(sample) = simplifile.read(from: "./src/day02/sample.txt")
  let assert Ok(input) = simplifile.read(from: "./src/day02/input.txt")

  assert part_a(sample) == 1_227_775_554
  echo #("part A:", part_a(input))

  assert is_sym(1) == False
  assert is_sym(11) == True
  assert is_sym(121) == False
  assert is_sym(123) == False
  assert is_sym(1111) == True
  assert is_sym(121_212) == True
  assert is_sym(824_824_824) == True

  assert part_b(sample) == 4_174_379_265
  echo #("part B:", part_b(input))
}
