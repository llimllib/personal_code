from collections import defaultdict


def run(inp, stop=2020, verbose=False):
    said = {j: i + 1 for i, j in enumerate(inp[:-1])}

    turn = len(inp) + 1
    current = inp[-1]
    while 1:
        if current not in said:
            if verbose:
                print(f"{turn}: 0 (adding {current} {dict(said.items())})")
            said[current] = turn - 1
            current = 0
        else:
            if verbose:
                print(
                    f"{turn}: {(turn-1) - said[current][0]} ({turn-1} - {said[current][0]}) {current} {dict(said.items())}"
                )
            c = (turn - 1) - said[current]
            said[current] = turn - 1
            current = c
        turn += 1
        if turn > stop:
            return current


assert run([1,3,2]) == 1
assert run([2,1,3]) == 10
assert run([1,2,3]) == 27
assert run([2,3,1]) == 78
assert run([3,2,1]) == 438
assert run([3,1,2]) == 1836

# assert run([0,3,6], stop=30000000) == 175594
print(run([1, 0, 18, 10, 19, 6], stop=2020))
print(run([1, 0, 18, 10, 19, 6], stop=30000000))
