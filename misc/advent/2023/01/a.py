import re


def n(line):
    ns = re.findall("([0-9])", line)
    return int(ns[0] + ns[-1])


names = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
}


def n2(line):
    ns = [
        names.get(x, x)
        for x in re.findall(
            "(?=([0-9]|one|two|three|four|five|six|seven|eight|nine|ten))", line
        )
    ]
    return int(ns[0] + ns[-1])


def part1(iter):
    return sum(n(line) for line in iter)


def part2(iter):
    return sum(n2(line) for line in iter)


assert (
    part1(
        """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet""".split()
    )
    == 142
)

print("part 1:", part1(open("input.txt")))

assert (
    part2(
        """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen""".split()
    )
    == 281
)

print("part 2:", part2(open("input.txt")))
