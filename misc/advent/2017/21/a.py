import numpy as np
from itertools import product


def go(rules_raw, iters=5):
    rules = {}
    for rule in rules_raw:
        a, b = rule.split("=>")
        frompat = np.array([
            list(map(int, list(x)))
            for x in a.replace("#", "1").replace('.', "0").strip().split(r'/')
        ]).tostring()
        topat = np.array([
            list(map(int, list(x)))
            for x in b.replace("#", "1").replace('.', "0").strip().split(r'/')
        ])
        rules[frompat] = topat

    grid = np.array([[0, 1, 0], [0, 0, 1], [1, 1, 1]])
    for iteration in range(iters):
        # print(grid, np.count_nonzero(grid), grid.shape)
        # print("-------------")
        if grid.shape[0] % 2 == 0:
            newdim = (grid.shape[0] // 2) * 3
        else:
            newdim = (grid.shape[0] // 3) * 4
        newgrid = np.zeros((newdim, newdim), dtype=int)
        split = 2 if grid.size % 2 == 0 else 3
        newsize = 4 if split == 3 else 3
        indices = product(range(grid.shape[0] // split), repeat=2)
        for i, j in indices:
            subgrid = grid[i * split:(i + 1) * split, j * split:(
                j + 1) * split]

            symmetries = [
                subgrid,
                np.flip(subgrid, 1),
                np.rot90(subgrid),
                np.flip(np.rot90(subgrid), 0),
                np.rot90(subgrid, 2),
                np.flip(np.rot90(subgrid, 2), 0),
                np.rot90(subgrid, 3),
                np.flip(np.rot90(subgrid, 3), 0)
            ]
            for symmetry in symmetries:
                if symmetry.tostring() in rules:
                    replacement = rules[symmetry.tostring()]
                    # print(f"found rule {symmetry} => {replacement}")
                    newgrid[i * newsize:(i + 1) * newsize, j * newsize:(
                        j + 1) * newsize] = replacement
                    break
        grid = newgrid
    print(grid, np.count_nonzero(grid), grid.shape)


if __name__ == "__main__":
    go(["../.# => ##./#../...", ".#./..#/### => #..#/..../..../#..#"], 2)
    go(list(open("input.txt")), 5)
    go(list(open("input.txt")), 18)
