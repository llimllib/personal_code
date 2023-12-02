import re


def maxi(line, color):
    return max(int(x) for x in re.findall(rf"(\d+) {color}", line))


print(
    sum(
        i + 1
        for i, line in enumerate(open("input.txt"))
        if maxi(line, "red") <= 12
        and maxi(line, "green") <= 13
        and maxi(line, "blue") <= 14
    )
)

print(
    sum(
        maxi(line, "red") * maxi(line, "blue") * maxi(line, "green")
        for line in open("input.txt")
    )
)
