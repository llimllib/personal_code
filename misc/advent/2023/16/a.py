import sys

Grid = list[list[str]]
Point = tuple[int, int]
Dir = int
Beam = tuple[Point, Dir]
Cache = set[Beam]

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


def valid(beam: Beam, maxrow: int, maxcol: int) -> bool:
    return 0 <= beam[0][0] < maxrow and 0 <= beam[0][1] < maxcol


def update_beam(grid: Grid, beam: Beam, cache: Cache) -> set[Beam]:
    if beam in cache:
        return set()

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

    cache.add(beam)

    return pbeams


def run(
    grid: Grid, beams: set[Beam], points: set[Point], cache: Cache
) -> tuple[set[Beam], set[Point]]:
    newbeams = {b for beam in beams for b in update_beam(grid, beam, cache)}

    for (row, col), _ in newbeams:
        points.add((row, col))

    return (newbeams, points)


def run_to_completion(grid, beams={((0, 0), R)}):
    cache = set()
    points = {pos for pos, _ in beams}
    n = 0
    lastn = [-1, -1, -1, -1, -1]
    while beams and n < 800:
        beams, points = run(grid, beams, points, cache)
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
print(
    "part 2:",
    max(
        len(run_to_completion(grid, {((row, col), dir)}))
        for (row, col), dir in [((row, 0), R) for row in range(maxrow)]
        + [((row, maxcol - 1), L) for row in range(maxrow)]
        + [((0, col), D) for col in range(maxcol)]
        + [((maxrow - 1, col), U) for col in range(maxcol)]
    ),
)
