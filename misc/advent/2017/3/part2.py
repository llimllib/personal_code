grid = {}

def neighbors(grid, x, y):
    return sum([grid.get((x-1,y), 0),
        grid.get((x+1,y), 0),
        grid.get((x,y+1), 0),
        grid.get((x,y-1), 0),
        grid.get((x-1,y+1), 0),
        grid.get((x+1,y+1), 0),
        grid.get((x+1,y-1), 0),
        grid.get((x-1,y-1), 0)])

RIGHT = 0
UP = 1
LEFT = 2
DOWN = 3

def fill(grid):
    grid[(0,0)] = 1
    grid[(1,0)] = 1

    x = 1
    y = 0
    direction = RIGHT

    while 1:
        if direction == RIGHT:
            if not grid.get((x, y+1)): direction = UP
            else:
                x += 1
                grid[(x, y)] = neighbors(grid, x, y)
                yield grid[(x, y)]
        if direction == UP:
            if not grid.get((x-1, y)): direction = LEFT
            else:
                y += 1
                grid[(x, y)] = neighbors(grid, x, y)
                yield grid[(x, y)]
        if direction == LEFT:
            if not grid.get((x, y-1)): direction = DOWN
            else:
                x -= 1
                grid[(x, y)] = neighbors(grid, x, y)
                yield grid[(x, y)]
        if direction == DOWN:
            if not grid.get((x+1, y)): direction = RIGHT
            else:
                y -= 1
                grid[(x, y)] = neighbors(grid, x, y)
                yield grid[(x, y)]

for i in fill(grid):
    if i > 289326:
        print(i)
        break
