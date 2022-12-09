map_ = [list(map(int, l)) for l in open("input.txt").read().strip().split()]
sample = [
    list(map(int, l))
    for l in """30373
25512
65332
33549
35390""".strip().split()
]


def visible(map_):
    count = 0
    height = len(map_)
    width = len(map_[0])
    for r, row in enumerate(map_):
        for c, _ in enumerate(row):
            tree = map_[r][c]
            # left
            for cc in range(c - 1, -1, -1):
                if map_[r][cc] >= tree:
                    break
            else:
                # print("l", r, c, tree)
                count += 1
                continue

            # right
            for cc in range(c + 1, height):
                if map_[r][cc] >= tree:
                    break
            else:
                count += 1
                # print("r", r, c, tree)
                continue

            # up
            for rr in range(r - 1, -1, -1):
                if map_[rr][c] >= tree:
                    break
            else:
                count += 1
                # print("u", r, c, tree)
                continue

            # down
            for rr in range(r + 1, width):
                if map_[rr][c] >= tree:
                    break
            else:
                count += 1
                # print("d", r, c, tree)
                continue
    return count


print(visible(sample))
print(visible(map_))


def viewscore(map_):
    max_ = 0
    height = len(map_)
    width = len(map_[0])
    for r, row in enumerate(map_):
        for c, _ in enumerate(row):
            tree = map_[r][c]
            l = g = u = d = 0
            # left
            for cc in range(c - 1, -1, -1):
                l += 1
                if map_[r][cc] >= tree:
                    break

            # right
            for cc in range(c + 1, height):
                g += 1
                if map_[r][cc] >= tree:
                    break

            # up
            for rr in range(r - 1, -1, -1):
                u += 1
                if map_[rr][c] >= tree:
                    break

            # down
            for rr in range(r + 1, width):
                d += 1
                if map_[rr][c] >= tree:
                    break

            score = l * g * d * u
            if score > max_:
                print(r, c, score)
                max_ = score

    return max_


print(viewscore(sample))
print(viewscore(map_))
