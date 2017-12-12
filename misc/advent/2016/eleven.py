import json
import sys
from copy import deepcopy

# stront = 1, plut = 2, thul = 3, ruth = 4, cur = 5
# positive = generator. negative = chip
# the first number is the elevator
initial_state = (0, [
    [1, -1, 2, -2],
    [3, 4, -4, 5, -5],
    [-3],
    [],
])

# def act(state, action):
#   return state after performing action
#
# def run(state, actions):
#   for legal action in state:
#     if isgoal(action): raise Exception("done {}".format(actions + action)
#     run(act(action, state), actions + action)

# return true if the item can be safely moved to the floor
def no_killer(floor, item):
    # if there's an un-paired generator and it's not the match
    # for this item, then the floor is safe for the item
    return not any(y for y in floor if -y not in floor and y > 0 and y != -item)

# not exactly right, but let's just say you can't move a generator solo unless the
# microchip is on a different floor. I don't think you ever need to do that in an
# optimal solution?
def can_move_generator(floor, item):
    return not any(y for y in floor if y == -item)

# an action is a pair of action and one or two items
# action: "U" or "D"
# items: a list of one or two items
def legal_actions(state):
    actions = []
    elevator, floors = state
    floor = floors[elevator]
    for item in floor:
        # microchips can go solo if there's not a killer on the next floor
        if item < 0:
            if elevator > 0 and no_killer(floors[elevator-1], item): actions.append(("D", [item], elevator))
            if elevator < 3 and no_killer(floors[elevator+1], item): actions.append(("U", [item], elevator))
        # generators can always go solo. But let's try only moving them up
        else:
            if elevator > 0 and can_move_generator(floor, item): actions.append(("D", [item], elevator))
            if elevator < 3 and can_move_generator(floor, item): actions.append(("U", [item], elevator))
        # safe pairs
        if item > 0 and -item in floor:
            # let's never move pairs downstairs. Can that ever help? I don't think so?
            #if elevator > 0: actions.append(("D", [-item, item], elevator))
            if elevator < 3: actions.append(("U", [-item, item], elevator))

    return actions

def goal(state):
    if map(len, state[1]) == [0,0,0,10]: return True

def lens(state):
    return list(map(len, state[1]))

def act(state, action):
    elevator, floors = state
    floors = deepcopy(floors)
    move, items, floor = action
    for item in items: floors[floor].remove(item)
    if move == "D":
        elevator -= 1
        floors[floor-1] += items
    else:
        elevator += 1
        floors[floor+1] += items
    return (elevator, floors)

def run(state, actions=[]):
    # bfs
    queue = []
    visited = set()

    depth = 0
    while 1:
        #print("-----")
        #print(state, actions)
        #print("xxxxxx")
        #input()
        depth += 1
        print("depth: {}".format(depth))
        statestr = json.dumps(state)
        if statestr not in visited:
            visited.add(statestr)
            for action in legal_actions(state):

                if goal(state): raise Exception("Found it! {} {}".format(len(actions), actions))
                newstate = act(state, action)
                #print(newstate, action)
                queue.append((newstate, actions + [action]))
        state, actions = queue.pop(0)
        #print(lens(state), state, queue)
        #input()

print(legal_actions(initial_state))
run(initial_state)
