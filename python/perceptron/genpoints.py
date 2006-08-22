import random

def gen_random_points(n):
    """generate n random linearly seperable points"""
    r = random.Random()
    points = []
    count = 0
    while count < n:
        x, y = (r.randint(0,99), r.randint(0,99))
        if 100 - x > y + 1:
            points.append(((x,y),-1))
            count += 1
        elif 100 - x < y - 1:
            points.append(((x,y),1))
            count += 1
    return points
