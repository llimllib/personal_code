from functools import reduce

print(sum(l[3 * i % (len(l) - 1)] == "#" for i, l in enumerate(open("input.txt"))))

print(
    reduce(
        lambda a, b: a * b,
        map(
            lambda a: sum(
                l[a[0] * (i // a[1]) % (len(l) - 1)] == "#" * (i % a[1] == 0)
                for i, l in enumerate(open("input.txt"))
            ),
            [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)],
        ),
        1,
    )
)
