import concurrent.futures
import sys
import copyreg
import types


# https://stackoverflow.com/a/32088533/42559
def _pickle_method(m):
    if m.im_self is None:
        return getattr, (m.im_class, m.im_func.func_name)
    else:
        return getattr, (m.im_self, m.im_func.func_name)


copyreg.pickle(types.MethodType, _pickle_method)

Grid = list[list[str]]
Point = tuple[int, int]
Dir = int
Beam = tuple[Point, Dir]

L, D, R, U = [1, 2, 3, 4]
mirror_a = {L: U, U: L, D: R, R: D}  # \
mirror_b = {L: D, U: R, D: L, R: U}  # /
advance = {
    U: lambda pos: ((pos[0] - 1, pos[1]), U),
    D: lambda pos: ((pos[0] + 1, pos[1]), D),
    L: lambda pos: ((pos[0], pos[1] - 1), L),
    R: lambda pos: ((pos[0], pos[1] + 1), R),
}


def pgrid(grid: Grid, points: set[Point], show_mirrors=True):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] == ".":
                sys.stdout.write("#" if (row, col) in points else ".")
            else:
                if show_mirrors:
                    sys.stdout.write(grid[row][col])
                else:
                    sys.stdout.write("#" if (row, col) in points else ".")
        sys.stdout.write("\n")


def valid(beam: Beam, maxrow: int, maxcol: int):
    return 0 <= beam[0][0] < maxrow and 0 <= beam[0][1] < maxcol


cache = {}


def update_beam(grid: Grid, beam: Beam) -> set[Beam]:
    if beam in cache:
        return cache[beam]

    (row, col), dir = beam
    newdir = dir
    pbeams = set()  # possible new beams
    maxrow = len(grid)
    maxcol = len(grid[0])
    match grid[row][col]:
        case "|" if dir in [R, L]:
            newdir = U
            pbeams.add(advance[D](beam[0]))
        case "-" if dir in [U, D]:
            newdir = L
            pbeams.add(advance[R](beam[0]))
        case "\\":
            newdir = mirror_a[dir]
        case "/":
            newdir = mirror_b[dir]

    pbeams.add(advance[newdir](beam[0]))
    pbeams = set(b for b in pbeams if valid(b, maxrow, maxcol))

    cache[beam] = pbeams

    return pbeams


def run(
    grid: Grid, beams: set[Beam], points: set[Point]
) -> tuple[set[Beam], set[Point]]:
    newbeams = {b for beam in beams for b in update_beam(grid, beam)}

    for (row, col), _ in newbeams:
        points.add((row, col))

    return (newbeams, points)


def run_to_completion(grid, beams={((0, 0), R)}):
    points = {pos for pos, _ in beams}
    n = 0
    lastn = [-1, -1, -1, -1, -1]
    while beams and n < 800:
        beams, points = run(grid, beams, points)
        if all(n == len(points) for n in lastn):
            break
        lastn.append(len(points))
        lastn = lastn[-5:]
        n += 1
    return points


grid = [list(line.strip()) for line in sys.stdin]
print("part 1:", len(run_to_completion(grid)))

maxrow = len(grid)
maxcol = len(grid[0])

# this is actually quite a bit slower
with concurrent.futures.ThreadPoolExecutor() as executor:
    print(
        "part 2:",
        max(
            executor.map(
                lambda args: len(
                    run_to_completion(args[0], {((args[1], args[2]), args[3])})
                ),
                [(grid, row, 0, R) for row in range(maxrow)]
                + [(grid, row, maxcol - 1, L) for row in range(maxrow)]
                + [(grid, 0, col, D) for col in range(maxcol)]
                + [(grid, maxrow - 1, col, U) for col in range(maxcol)],
            )
        ),
    )
