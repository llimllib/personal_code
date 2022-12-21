def monkeval(op):
    try:
        return int(op)
    except ValueError:
        return op.strip().split(" ")


def parse(text):
    return {
        name: monkeval(op) for name, op in [l.split(":") for l in text.split("\n") if l]
    }


def run(monkeys, name):
    if isinstance(monkeys[name], (int, str)):
        return monkeys[name]
    else:
        l, op, r = monkeys[name]
        eq = f"({run(monkeys, l)} {op} {run(monkeys, r)})"
        if "x" not in eq:
            return eval(eq)
        return eq


def search(monkeys):
    monkeys["humn"] = "x"
    print(run(monkeys, monkeys["root"][0]), "==", run(monkeys, monkeys["root"][2]))


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
search(parse(sample))
search(parse(open("input.txt").read()))

# prints out an equation that I plugged into Sage:

# x = var('x')
# solve(
# ((((215084115331988.0 - (4 * (((((((2 * ((2 * (781.0 + ((925 + ((4 * (((207 + (((((4 * (646 + (((( (8 * (((889.0 + (622.0 +
#  (((((968.0 + ((2 * ((2 * ((((((2 * ((((((12 * ((((((((318 * (768 + ((x - 876.0) / 9))) + 325) / 11) - 918) * 2) + 798.0) / 2)
#  - 905)) - 752) / 2) + 688) * 2) + 702)) - 442 ) / 2) + 579.0) / 4) - 231)) - 137.0)) + 103)) / 9) + 50) * 2) - 39))) / 6) + 194)
#  ) - 106.0) / 2) - 69.0) / 2))) + 314) / 2) - 938) * 7)) / 6) - 6)) - 895.0)) / 2))) + 789)) - 863.0) / 3) - 606) +  201) / 2) + 8))) / 4) + 708.0) * 2) == 48165982835110, x)
