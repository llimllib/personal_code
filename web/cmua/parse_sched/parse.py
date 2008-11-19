f = file("2008.csv")

def line():
    return f.readline().split(',')

class Team():
    def __init__(self, id, name, captain, games):
        self.id = id
        self.name = name
        self.captain = captain
        self.games = games

    def __str__(self): return "%s %s %s" % (self.id, self.name, self.captain)
    def __repr__(self): return self.__str__()

def getteam(id):
    global teams
    for team in teams:
        if team.id == id:
            return team

def clean(line):
    return [x.strip('" ') for x in line]

dates = line()[4:20:2]
_ = line()
teams = []
for i in range(16):
    t1 = clean(line())
    t2 = clean(line())
    games = dict(zip(dates, zip(zip(t1[4:18:2], t1[5:18:2]), 
                                zip(t2[4:18:2], t2[5:18:2]))))
    teams.append(Team(t1[1], t1[2], t2[2], games))

for i in range(5): line()

def tripleheader(field, dates, part_line):
    games = [(a.strip('" '), b.strip('" ')) for a,b in
                    [x.split('v') for x in part_line]]
    for date, game in zip(dates, games):
        t_a, t_b = game
        teama, teamb = getteam(t_a), getteam(t_b)
        assert(teama and teamb)
        teama.games[date] = teama.games[date] + ((str(teamb.id), field),)
        teamb.games[date] = teamb.games[date] + ((str(teama.id), field),)


l1 = line()[6:12:2]
tripleheader("B3", ['9-Jun', '16-Jun', '23-Jun'], l1)
l2 = line()[6:12:2]
tripleheader("B4", ['9-Jun', '16-Jun', '23-Jun'], l2)
l3 = line()[8:12:2]
tripleheader("B5", ['16-Jun', '23-Jun'], l3)

for team in teams:
    #make sure we added a third game for all teams
    assert(max(len(gameday) for gameday in team.games.itervalues()) == 3)

for team in teams:
    #TODO print out sql statements
    pass
