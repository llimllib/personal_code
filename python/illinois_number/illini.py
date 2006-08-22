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

for line in f:
    line = line.lower()
    team1, score1 = line[11:34].strip(), int(line[34:37])
    team2, score2 = line[38:61].strip(), int(line[61:64])
    winner = max((score1, team1), (score2, team2))
    loser = min((score1, team1), (score2, team2))
    teams.add(team1)
    teams.add(team2)
    games.append((winner, loser))

for i, (winner, loser) in enumerate(games):
    if loser[1] == team_to_beat:
        del games[i]
        root = Node(winner, loser, None, 0)

def print_path(node):
    print node.winner, node.loser
    if node.parent:
        print_path(node.parent)

def nodes(depth, root):
    #yield all nodes at depth i
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

for depth in range(100):
    print "at depth %d" % depth
    for n in nodes(depth, root):
        if not 1 and n.winner[1] == team_to_find:
            print_path(n)
            raise "found"
        deletions = []
        for i, (winner, loser) in enumerate(games):
            if loser[1] == n.winner[1]:
                n.addchild(Node(winner, loser, n, depth+1))
                deletions.append(i)
        deletions.reverse()
        for i in deletions:
            del games[i]

def find_team(team, root):
    queue = deque()
    queue.append(root)
    pop = queue.popleft #bfs
    while queue:
        node = pop()
        if team == node.winner[1]:
            return node
        else:
            for n in node.children:
                queue.append(n)

def path_string(node):
    path = [] 
    while node.parent:
        wscore, win = node.winner
        lscore, lose = node.loser
        path.append('%s beat %s %s - %s\n' % (win, lose, wscore, lscore))
        node = node.parent
    wscore, win = node.winner
    lscore, lose = node.loser
    path.append('%s beat %s %s - %s\n' % (win, lose, wscore, lscore))
    return ''.join(path)

#team_paths = {}
#fout = file('team_paths.py', 'w')
#fout.write('team_paths = {\n')
#for team in teams:
#    node = find_team(team, root)
#    if node:
#        fout.write('"%s": """%s""",\n' % (team, path_string(node)))
#    else:
#        fout.write('"%s": "This team has no Illinois number",\n' % team)
#fout.write('0:0}')
