from utils import openbits


def todec(cs):
    return int("".join(map(str, cs)), 2)


def summ(ns):
    gamma = []
    epsilon = []
    l = len(ns)
    for pos in map(sum, zip(*ns)):
        if pos > l / 2:
            gamma.append(1)
            epsilon.append(0)
        else:
            gamma.append(0)
            epsilon.append(1)
    gamma, epsilon = map(todec, (gamma, epsilon))
    return gamma, epsilon, gamma * epsilon


print(summ(list(openbits("small.txt"))))
print(summ(list(openbits("input.txt"))))


def oxyrating(ns):
    for col in range(len(ns)):
        if len(ns) == 1:
            break
        digits = [row[col] for row in ns]
        if digits.count(1) < len(digits) / 2:
            ns = [n for n in ns if n[col] == 0]
        else:
            ns = [n for n in ns if n[col] == 1]
    return todec(ns[0])


def co2rating(ns):
    for col in range(len(ns)):
        if len(ns) == 1:
            break
        digits = [row[col] for row in ns]
        if digits.count(1) < len(digits) / 2:
            ns = [n for n in ns if n[col] == 1]
        else:
            ns = [n for n in ns if n[col] == 0]
    return todec(ns[0])


sml = list([list(x) for x in openbits("small.txt")])
inp = list([list(x) for x in openbits("input.txt")])
print(oxyrating(sml) * co2rating(sml))
print(oxyrating(inp) * co2rating(inp))
