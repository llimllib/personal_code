from pprint import pprint as pp
import ipdb


def parse(f):
    lines = []
    for line in open(f):
        if not line:
            break
        inp, outp = line.strip().split("|")
        ins = inp.strip().split(" ")
        assert all(len(x) for x in ins)
        outs = outp.strip().split(" ")
        lines.append((ins, outs))
    return lines


def uniques(lines):
    uniques = 0
    for _, outs in lines:
        for o in outs:
            if len(o) in [2, 3, 4, 7]:
                uniques += 1

    return uniques


print(uniques(parse("small.txt")))
print(uniques(parse("input.txt")))

digits = {
    frozenset("abcefg"): "0",
    frozenset("cf"): "1",
    frozenset("acdeg"): "2",
    frozenset("acdfg"): "3",
    frozenset("bcdf"): "4",
    frozenset("abdfg"): "5",
    frozenset("abdefg"): "6",
    frozenset("acf"): "7",
    frozenset("abcdefg"): "8",
    frozenset("abcdfg"): "9",
}


def solve2(lines):
    key = {}
    sum_ = 0
    for ins, outs in lines:
        ins = list(map(set, sorted(ins, key=len)))

        # one must be the first element
        one = ins[0]
        seven = ins[1]
        four = ins[2]

        # we know 3 is represented by the one with 5 characters that has both
        # values from the 1 in it
        three = [x for x in (ins[3], ins[4], ins[5]) if one <= x][0]

        # six is the one with 6 chars that doesn't include one
        six = [x for x in (ins[6], ins[7], ins[8]) if not one <= x][0]

        # zero is not six and does not overlap four
        zero = [x for x in (ins[6], ins[7], ins[8]) if x != six and len(four - x)][0]

        # five and two are the ones with 5 characters that aren't three
        five_or_two = [x for x in (ins[3], ins[4], ins[5]) if x != three]

        # five is the one of those that is a subset of 6, and two the other
        five = [x for x in five_or_two if x <= six][0]
        two = [x for x in five_or_two if x != five][0]

        # a = seven - one
        key["a"] = ins[1] - ins[0]

        # now f is the one that's in five, not in two, and in one
        key["f"] = (five - two) & one
        key["c"] = (two - five) & one
        key["b"] = five - two - one
        key["e"] = two - five - one

        # d is in four but not in zero
        key["d"] = four - zero

        # g is five minus (four plus seven)
        key["g"] = five - (four | seven)

        for k in key:
            assert len(key[k]) == 1
            key[k] = key[k].pop()

        # make a translation table by inverting the key
        table = str.maketrans({v: k for k, v in key.items()})

        # for every output number, translate it using the key, and join them
        # all into a digit string, then turn that into an int and add it to the
        # sum
        sum_ += int(
            "".join(digits[frozenset(digit.translate(table))] for digit in outs)
        )

    print(sum_)


solve2(
    [
        (
            [
                "acedgfb",
                "cdfbe",
                "gcdfa",
                "fbcad",
                "dab",
                "cefabd",
                "cdfgeb",
                "eafb",
                "cagedb",
                "ab",
            ],
            ["cdfeb", "fcadb", "cdfeb", "cdbaf"],
        ),
    ]
)

solve2(parse("input.txt"))
