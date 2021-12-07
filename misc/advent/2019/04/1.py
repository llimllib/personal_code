def valid(pw: str):
    dupe = False
    for i, c in enumerate(pw):
        if i > 0 and pw[i - 1] == c:
            dupe = True
        if i > 0 and c < pw[i - 1]:
            return False
    return dupe


print(valid("111111"))
print(valid("223450"))
print(valid("123789"))
print(valid("222223"))

# 168630-718098
r = range(168630, 718099)
valids = list(p for p in r if valid(str(p)))
print(len(valids))
