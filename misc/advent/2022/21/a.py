from operator import mul, floordiv, add, sub

ops = {
    "+": add,
    "-": sub,
    "*": mul,
    "/": floordiv,
}


def monkeval(op):
    try:
        return int(op)
    except ValueError:
        parts = op.strip().split(" ")
        return (ops[parts[1]], parts[0], parts[2])


def parse(text):
    return {
        name: monkeval(op) for name, op in [l.split(":") for l in text.split("\n") if l]
    }


def run(monkeys, name):
    print(name, monkeys[name])
    if isinstance(monkeys[name], int):
        return monkeys[name]
    else:
        return monkeys[name][0](
            run(monkeys, monkeys[name][1]), run(monkeys, monkeys[name][2])
        )


sample = """root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32"""
res = run(parse(sample), "root")
assert res == 152, res

res = run(parse(open("input.txt").read()), "root")
print("part 1:", res)
