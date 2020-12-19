from operator import mul, add


def evil(code):
    val = None
    op = None
    stack = []
    for char in code:
        if char.isdigit():
            n = int(char)
            if op:
                val = op(val, n)
            else:
                val = n
        elif char == "*":
            op = mul
        elif char == "+":
            op = add
        elif char == "(":
            stack.append((val, op))
            val = None
            op = None
        elif char == ")":
            n = val
            val, op = stack.pop()
            if val:
                val = op(val, n)
            else:
                val = n
    return val


assert evil("2 * 3 + (4 * 5)") == 26
assert evil("5 + (8 * 3 + 9 + 3 * 4 * 3)") == 437
assert evil("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))") == 12240
assert evil("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") == 13632

print(sum(evil(line) for line in open("input.txt")))
