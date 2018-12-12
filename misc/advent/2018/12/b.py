import sys


def sumstate(state):
    sum = 0
    for i in range(len(state)):
        if state[i] == "#":
            sum += i - 10000
    return sum


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
    state = ['.'] * 10000 + state + ['.'] * 10000
    generation = 0
    prevscore = 0
    while 1:
        generation += 1
        if (generation % 100 == 0):
            score = sumstate(state)
            diff = score - prevscore
            prevscore = score
            sys.stdout.write(f"{generation}({score} {diff})\n")
            sys.stdout.flush()
        nextstate = []
        for i in range(len(state)):
            hood = ''.join(state[i - 2:i + 3])
            if hood in rules:
                nextstate.append(rules[hood])
            else:
                nextstate.append('.')
        if state == nextstate:
            break
        state = nextstate
    print()
    print(f"{generation}: {''.join(state).strip('.')}")
    print(sumstate(state))


# Based on this (generation, score, diff):
# 100(9030 9030)
# 200(15999 6969)
# 300(22899 6900)
# 400(29799 6900)
# 500(36699 6900)
# 600(43599 6900)
# 700(50499 6900)
# 800(57399 6900)
# 900(64299 6900)
# 1000(71199 6900)
#
# I thought this would be right, but it's too low:
# In [42]: 71199 + (69 * (50000000000 - 1000))
# Out[42]: 3450000002199
#
# it's within a factor of 100 though (this one was too high):
# In [39]: 71199 + (6900 * (50000000000 - 1000))
# Out[39]: 344999993171199
#
# I wonder if I'm off by one somewhere or something?
# yup!
# 3450000002199 + 69 = 3450000002268 is the correct answer

if __name__ == "__main__":
    # do(open("small.txt"))
    do(open("input.txt"))
