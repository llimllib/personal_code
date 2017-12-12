path = "R5, L2, L1, R1, R3, R3, L3, R3, R4, L2, R4, L4, R4, R3, L2, L1, L1, R2, R4, R4, L4, R3, L2, R1, L4, R1, R3, L5, L4, L5, R3, L3, L1, L1, R4, R2, R2, L1, L4, R191, R5, L2, R46, R3, L1, R74, L2, R2, R187, R3, R4, R1, L4, L4, L2, R4, L5, R4, R3, L2, L1, R3, R3, R3, R1, R1, L4, R4, R1, R5, R2, R1, R3, L4, L2, L2, R1, L3, R1, R3, L5, L3, R5, R3, R4, L1, R3, R2, R1, R2, L4, L1, L1, R3, L3, R4, L2, L4, L5, L5, L4, R2, R5, L4, R4, L2, R3, L4, L3, L5, R5, L4, L2, R3, R5, R5, L1, L4, R3, L1, R2, L5, L1, R4, L1, R5, R1, L4, L4, L4, R4, R3, L5, R1, L3, R4, R3, L2, L1, R1, R2, R2, R2, L1, L1, L2, L5, L3, L1"

def parsepath(path):
    loc = [0,0]
    di = 0 # 0N 1E 2S 3W
    for p in path.split(', '):
        move = p[0]
        n = int(p[1:])
        di = (di+1 if move == 'R' else di-1) % 4
        if di == 0: loc[1] += n
        if di == 1: loc[0] += n
        if di == 2: loc[1] -= n
        if di == 3: loc[0] -= n

    return sum(map(abs, loc))

def parsesteps(path):
    path = [p.strip() for p in path.split(',')]
    loc = [0,0]
    locs = {(0,0)}
    di = 0 # 0N 1E 2S 3W
    for p in path:
        move = p[0]
        n = int(p[1:])
        di = (di+1 if move == 'R' else di-1) % 4
        if di == 0:
            steps = {(loc[0], loc[1]+i) for i in range(1,n+1)}
            if steps & locs:
                for step in steps:
                    if step in locs:
                        raise Exception("dupe loc {}\n{}".format(step, locs))
            locs = locs | steps
            loc[1] += n
        if di == 1:
            steps = {(loc[0]+i, loc[1]) for i in range(1,n+1)}
            if steps & locs:
                for step in steps:
                    if step in locs:
                        raise Exception("dupe loc {}\n{}".format(step, locs))
            locs = locs | steps
            loc[0] += n
        if di == 2:
            steps = {(loc[0], loc[1]-i) for i in range(1,n+1)}
            if steps & locs:
                for step in steps:
                    if step in locs:
                        raise Exception("dupe loc {}\n{}".format(step, locs))
            locs = locs | steps
            loc[1] -= n
        if di == 3:
            steps = {(loc[0]-i, loc[1]) for i in range(1,n+1)}
            if steps & locs:
                for step in steps:
                    if step in locs:
                        raise Exception("dupe loc {}\n{}".format(step, locs))
            locs = locs | steps
            loc[0] -= n
