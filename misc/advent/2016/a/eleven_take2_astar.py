from heapq import heappush, heappop
import itertools
import sys

# A state is a tuple (current elevator floor <int>, plan [[string]])
INITIAL_STATE = (0,
                 (frozenset(("SG", "SM", "PG", "PM", "EG", "EM", "DG", "DM")),
                  frozenset(("TG", "RG", "RM", "CG", "CM")),
                  frozenset(("TM",)),
                  frozenset()))

VALID_CACHE = {}
STATE_CACHE = set()


def isvalid(state):
    """return true if the state is valid"""
    if state in VALID_CACHE:
        return VALID_CACHE[state]

    plan = state[1]
    for floor in plan:
        for item in floor:
            if item[1] == "M":
                if item[0] + "G" not in floor:
                    # the microchip is on a floor without its generator. This
                    # is safe unless there is another generator on the floor
                    for i in floor:
                        if i[1] == "G":
                            VALID_CACHE[state] = False
                            return False

    VALID_CACHE[state] = True
    return True


def isgoal(state):
    """return true if we've reached the goal state"""
    return len(state[1][3]) == 10


def makemove(state, items, action):
    """
    return a new state, moving items<frozenset<string>> by action<enum(u, d)>
    in state<state>
    """
    floor, plan = state
    newfloor = floor + 1 if action == "u" else floor - 1
    newplan = []
    for leveln, level in enumerate(plan):
        if leveln == floor:
            newplan.append(level - items)
        elif leveln == newfloor:
            newplan.append(level | items)
        else:
            newplan.append(level)

    return (newfloor, tuple(newplan), depth + 1)


def validmoves(state):
    """return all valid states one move from a given state"""
    global STATE_CACHE
    if state in STATE_CACHE:
        # If we've already seen this state, there is no need to continue
        # searching it
        return set()
    STATE_CACHE.add(state)

    actions = {0: ["u"], 1: ["u", "d"], 2: ["u", "d"], 3: ["d"]}
    floor, plan = state
    newstates = set()

    # one item case
    for item in plan[floor]:
        for action in actions[floor]:
            items = frozenset((item,))
            maybe_state = makemove(state, items, action)
            if isvalid(maybe_state):
                newstates.add(maybe_state)

    # two item case
    for items in itertools.combinations(plan[floor], 2):
        for action in actions[floor]:
            maybe_state = makemove(state, frozenset(items), action)
            if isvalid(maybe_state):
                newstates.add(maybe_state)

    return newstates


def heuristic(state):
    _, levels = state
    return len(levels[2]) + 2 * len(levels[1]) + 4 * len(levels[0]) #+ depth**1.3


def pp(state):
    sys.stdout.write("{}: ".format(state[2]))
    for n, level in enumerate(state[1]):
        sys.stdout.write("{}: ".format(n))
        sys.stdout.write(",".join(level) + " ")
    sys.stdout.write("\n")
    sys.stdout.flush()

def main():
    """do stuff"""
    frontier = [(0, INITIAL_STATE)]

    # I'm guessing the answer is something < 30?
    maxdepth = 0
    while 1:
        _, state = heappop(frontier)
        if state[2] > maxdepth:
            pp(state)
            maxdepth = state[2]
        if state[2] > 100:
            import ipdb; ipdb.set_trace()
        if isgoal(state):
            print(f"solution depth: {state[2]}")
            return
        for newstate in validmoves(state):
            heappush(frontier, (heuristic(newstate), newstate))

if __name__=="__main__":
    main()

# XXX: can we exploit symmetry on this problem? i.e. if we have:
# [AM, AG, BM, BG] on floor 1, then the moves (AM, AG) up and (BM, BG) up are equivalent
