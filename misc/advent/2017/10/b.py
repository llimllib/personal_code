def rev(lst):
    return list(reversed(lst))


def go(size, ns):
    lst = list(range(size))
    pos = 0
    skip = 0
    for n in ns:
        end = min(size - 1, pos + n)
        l1 = rev(lst[pos:end + 1])
        import ipdb
        ipdb.set_trace()
        if pos + n > size - 1:
            x = (pos + n) - size
            l2 = rev(lst[:x])
            lst = l2 + lst[x:pos] + l1
        else:
            lst = lst[0:pos] + l1 + lst[end + 1:]
        pos += skip + n
        skip += 1
        print(lst)


if __name__ == "__main__":
    go(5, [3, 4, 1, 5])
    #go(256, map(int, open("input.txt").read().split(",")))
