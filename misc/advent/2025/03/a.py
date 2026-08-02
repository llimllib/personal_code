def getnumber(bank):
    max = "/"
    idx = -1
    for i, jolt in enumerate(bank[0:-1]):
        if jolt > max:
            idx = i
            max = jolt

    second = "/"
    for jolt in bank[idx + 1 :]:
        if jolt > second:
            second = jolt

    return int(max + second)


def parttwo(bank):
    print(bank)
    ns = []
    idx = -1
    for slice in range(12, -1, -1):
        max = "/"
        maxidx = -1
        print(list(bank[idx + 1 : -slice]))
        for i, jolt in enumerate(bank[idx + 1 : -slice]):
            if jolt > max:
                max = jolt
                maxidx = i
        idx += maxidx + 1
        print("idx", idx, "maxidx", maxidx, "max", max)
        ns.append(max)
    return int("".join(ns))


print(sum(list(map(getnumber, open("sample.txt").read().strip().split("\n")))))
print(sum(list(map(getnumber, open("input.txt").read().strip().split("\n")))))

print(list(map(parttwo, open("sample.txt").read().strip().split("\n"))))
