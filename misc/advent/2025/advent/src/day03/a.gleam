import gleam/int
import gleam/list
import gleam/result
import gleam/string
import simplifile

pub fn int(n: String) -> Int {
  int.parse(n) |> result.lazy_unwrap(fn() { panic as "invalid integer" })
}

pub fn parse(input: String) -> List(List(Int)) {
  string.trim(input)
  |> string.split(on: "\n")
  |> list.map(fn(line) { string.split(line, on: "") |> list.map(int) })
}

pub fn max_not_last(l: List(Int)) -> #(Int, Int) {
  case l {
    [] -> panic as "must be nonempty"
    [first, ..rest] -> max_not_last_loop(rest, first, 0, 0)
  }
}

// return the index of the largest element that's  not last
pub fn max_not_last_loop(
  l: List(Int),
  max: Int,
  cur_idx: Int,
  max_idx: Int,
) -> #(Int, Int) {
  // echo #(l, max, cur_idx, max_idx)
  case l {
    [] -> #(max, 0)
    [first, ..rest] ->
      case first > max, rest == [] {
        _, True -> #(max, max_idx)
        True, _ -> max_not_last_loop(rest, first, cur_idx + 1, cur_idx + 1)
        False, _ -> max_not_last_loop(rest, max, cur_idx + 1, max_idx)
      }
  }
}

// return the largest element whose index is >i
pub fn max_gt_index(l: List(Int), min_idx: Int) {
  case l, list.length(l) <= min_idx {
    _, True -> panic as "invalid idx"
    [], _ -> panic as "must be nonempty"
    [first, ..rest], _ -> max_gt_index_loop(rest, first, 0, min_idx)
  }
}

// precondition: cur_idx < len(l)
pub fn max_gt_index_loop(
  l: List(Int),
  max: Int,
  cur_idx: Int,
  min_idx: Int,
) -> Int {
  echo #(l, max, cur_idx, min_idx)
  case l {
    [] -> max
    [first, ..rest] ->
      case first > max, cur_idx + 1 == min_idx {
        _, True -> max_gt_index_loop(rest, first, cur_idx + 1, min_idx)
        True, _ -> max_gt_index_loop(rest, first, cur_idx + 1, min_idx)
        False, _ -> max_gt_index_loop(rest, max, cur_idx + 1, min_idx)
      }
  }
}

pub fn part_a(input) {
  parse(input)
  |> list.map(fn(row) {
    let #(max, max_idx) = max_not_last(row)
    let two = max_gt_index(row, max_idx)
    echo #(row, int.to_string(max) <> int.to_string(two), max_idx)
    { int.to_string(max) <> int.to_string(two) } |> int
  })
  |> echo
  |> list.fold(0, int.add)
}

pub fn main() {
  let assert Ok(sample) = simplifile.read(from: "./src/day03/sample.txt")
  // let assert Ok(input) = simplifile.read(from: "./src/day03/input.txt")

  echo part_a(sample)
}
