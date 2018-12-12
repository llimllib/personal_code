def do(infile):
    state = []
    rules = {}
    for line in infile:
        if 'initial state' in line:
            state = list(line.split()[-1])
        elif '=>' in line:
            rule, _, res = line.split()
            rules[rule] = res

    zeroidx = 100
    state = ['.'] * 100 + state + ['.'] * 100
    generations = 20
    for generation in range(generations):
        print(f"{generation}: {''.join(state).strip('.')}")
        nextstate = []
        for i in range(len(state)):
            hood = ''.join(state[i - 2:i + 3])
            if hood in rules:
                nextstate.append(rules[hood])
            else:
                nextstate.append('.')
        state = nextstate
    print(f"{generations}: {''.join(state).strip('.')}")
    sum = 0
    for i in range(len(state)):
        if state[i] == "#":
            sum += i - 100
    print(sum)


if __name__ == "__main__":
    do(open("small.txt"))
    do(open("input.txt"))
