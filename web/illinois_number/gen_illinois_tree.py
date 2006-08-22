#!/usr/bin/python
from collections import deque

f = file('cbbga.txt')

teams = set()
games = []
levels = {}
team_to_beat = 'illinois'
team_to_find = 'penn st.'
root = None

class Node:
    def __init__(self, winner, loser, parent, depth):
        self.winner = winner
        self.loser = loser
        self.parent = parent
        self.children = [] #teams that beat the winner
        self.depth = depth
        self._mark = 0

    def addchild(self, child):
        self.children.append(child)

#parse the file into a list of game tuples of the format:
#((win_score, winner), (lose_score, loser))
#where the scores are ints and the winner and loser are team name strings
for line in f:
    line = line.lower()
    team1, score1 = line[11:34].strip(), int(line[34:37])
    team2, score2 = line[38:61].strip(), int(line[61:64])
    winner = max((score1, team1), (score2, team2))
    loser = min((score1, team1), (score2, team2))
    games.append((winner, loser))

def print_path(node):
    """starting from a node, print all the way up to Ohio St - Illinois"""
    print node.winner, node.loser
    if node.parent:
        print_path(node.parent)

def nodes(depth, root):
    """yield all nodes at depth i, breadth-first"""
    queue = deque()
    queue.append(root)
    pop = queue.popleft #bfs
    while queue:
        node = pop()
        if node.depth == depth:
            yield node
        else:
            for n in node.children:
                queue.append(n)

#create a root node
for i, (winner, loser) in enumerate(games):
    if loser[1] == team_to_beat:
        del games[i]
        root = Node(winner, loser, None, 0)

#create the game tree
for depth in range(100):
    print "at depth %d" % depth
    for n in nodes(depth, root):
        if not 1 and n.winner[1] == team_to_find:
            print_path(n)
            raise "found"
        deletions = []
        for i, (winner, loser) in enumerate(games):
            #if the team at node n lost this game
            if loser[1] == n.winner[1]:
                #add the team that beat them to the tree
                n.addchild(Node(winner, loser, n, depth+1))

                #and we can delete this game, since any ramifications of it
                #will be played out underneath it in the tree
                deletions.append(i)
        deletions.reverse()
        for i in deletions:
            del games[i]
