import sympy


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
            return int(eval(eq))
        return eq


def gen(monkeys):
    monkeys["humn"] = "x"
    return f'{run(monkeys, monkeys["root"][0])} - {run(monkeys, monkeys["root"][2])}'


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
eq = gen(parse(sample))
x = sympy.symbols("x")
assert eval(f"sympy.solve({eq}, x)")[0] == 301
eq = gen(parse(open("input.txt").read()))
print("part 2:", eval(f"sympy.solve({eq}, x)")[0])
