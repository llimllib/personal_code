import sys
from shapely.geometry import Polygon

moves = []
for line in sys.stdin:
    dir, dist, color = line.strip().split(" ")
    moves.append((dir, int(dist), color.strip("()#")))

#        R        L         U             D
dirs = {"0": 1j, "2": -1j, "3": -1 + 0j, "1": 1 + 0j}
points = [0j]
depth = 0
for _, _, color in moves:
    dist, dir = int(color[:-1], 16), dirs[color[-1]]
    cur = points[-1]
    depth += dist - 1
    points.append(cur + dir * dist)

poly = Polygon((p.real, p.imag) for p in points)
print("buf:", poly.buffer(0.5, cap_style="square", join_style="mitre").area)
with open("final.svg", "w") as fout:
    pad = 2000
    minx = round(min(p.real for p in points))
    maxx = round(max(p.real for p in points))
    miny = round(min(p.imag for p in points))
    maxy = round(max(p.imag for p in points))
    fout.write(
        f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="{minx-pad} {miny-pad} {(maxx-minx)+pad} {(maxy-miny)+pad}">\n'
    )
    fout.write(poly.svg())
    fout.write("\n</svg>")
