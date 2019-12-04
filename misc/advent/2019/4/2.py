from collections import Counter


def valid(pw: str):
    for i, c in enumerate(pw):
        if i > 0 and c < pw[i - 1]:
            return False
    return 2 in Counter(pw).values()


print(valid("111111"))
print(valid("223450"))
print(valid("123444"))
print(valid("123445"))
print(valid("123789"))
print(valid("222223"))
print(valid("111122"))

# 168630-718098
r = range(168630, 718099)
valids = list(p for p in r if valid(str(p)))
print(len(valids))
