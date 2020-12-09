def calclegal(ns):
    legal = []
    for i in ns:
        for j in ns:
            legal.append(i + j)
    return legal


def main(f, npreamble=25):
    lines = [int(l) for l in open(f)]
    preamble = lines[:npreamble]
    body = lines[npreamble:]

    legal = calclegal(preamble)

    nlegal = 0
    illegal = 0
    for b in body:
        if b in legal:
            nlegal += 1
            # print(f"{b} is legal {legal}")
        else:
            illegal += 1
            print(f"{b} is illegal {legal}")
        preamble.pop(0)
        preamble.append(b)
        legal = calclegal(preamble)

    print(nlegal, illegal)


if __name__ == "__main__":
    # main("small.txt", 5)
    main("input.txt", 25)
