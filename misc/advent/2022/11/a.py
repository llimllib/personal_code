import re


class Monkey:
    def __init__(self, n, items, operation, testn, truemonkey, falsemonkey, monkeys):
        self.n = n
        self.items = items
        self.operation = eval(f"lambda old: {operation}")
        self.testn = testn
        self.truemonkey = truemonkey
        self.falsemonkey = falsemonkey
        self.monkeys = monkeys
        self.inspections = 0

    def run(self):
        while self.items:
            self.inspections += 1
            it = self.items.pop(0)
            it = self.operation(it) // 3
            if it % self.testn == 0:
                self.monkeys[self.truemonkey].items.append(it)
            else:
                self.monkeys[self.falsemonkey].items.append(it)

    def __str__(self):
        return f"{self.n}: {self.items}"

    def __repr__(self):
        return f"{self.n}: {self.items}"


lines = iter(open("input.txt").read().strip().split("\n"))
# lines = iter(open("sample.txt").read().strip().split("\n"))
monkeys = []
for line in lines:
    if line.startswith("Monkey"):
        n = int(line.split(" ")[-1].strip(":"))
        items = [int(x) for x in re.findall("\\d+", next(lines))]
        operation = next(lines).split("=")[1].strip()
        testn = int(next(lines).split(" ")[-1])
        truemonkey = int(next(lines).split(" ")[-1])
        falsemonkey = int(next(lines).split(" ")[-1])
        monkeys.append(
            Monkey(n, items, operation, testn, truemonkey, falsemonkey, monkeys)
        )

for _ in range(20):
    for monkey in monkeys:
        monkey.run()


def product(args):
    return args[0] * args[1]


print(monkeys)
print(product(list(sorted([m.inspections for m in monkeys]))[-2:]))
