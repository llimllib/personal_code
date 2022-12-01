print(
    sorted(
        [
            sum(map(int, l.split("\n")))
            for l in open("input.txt").read().strip().split("\n\n")
        ]
    )[-1]
)
print(
    sum(
        sorted(
            [
                sum(map(int, l.split("\n")))
                for l in open("input.txt").read().strip().split("\n\n")
            ]
        )[-3:]
    )
)
