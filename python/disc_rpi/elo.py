#!/usr/bin/python
import mx.DateTime, math, Gnuplot

class Game:
    def __init__(self, winner, loser, winner_points, loser_points, date):
        self.winner = winner
        self.loser = loser
        self.date = date
        self.wpoints = winner_points
        self.lpoints = loser_points
        ####################
        #elo calculation
        #http://www.mratings.com/theory/sauceda.htm
        ####################
        #point diff
        self.pd = winner_points - loser_points
        #point diff value: lower connotes higher importance of pd value
        self.pdv = 4.5
        #game point division
        #note that the winner is guaranteed >= .632 points, loser gets the rest
        self.gp_l = .4 ** (1 + self.pd / self.pdv)
        self.gp = 1 - self.gp_l

class Team:
    def __init__(self, name):
        self.name = name
        self.rating = 1000    #initial rankings are all 1000
        #TODO: tweak this k value
        self.k = 40          #rate of convergence
        self.opp_rating = 0
        self.games = 0
        self.game_list = []
        self.all_ratings = [1000]

    def update_rating(self, gp, we):
        self.rating = self.rating + self.k * (gp - we)
        self.all_ratings.append(self.rating)

def nmonth(m):
    if m == "jan": return 1
    if m == "feb": return 2
    if m == "mar": return 3
    if m == "apr": return 4
    if m == "may": return 5
    if m == "jun": return 6
    if m == "jul": return 7
    if m == "aug": return 8
    if m == "sep": return 9
    if m == "oct": return 10
    if m == "nov": return 11
    if m == "dec": return 12

def team_exists(team, teams):
    for t in teams:
        if t.name == team: return t
    else:
        teams.append(Team(team))
        return teams[-1]

def parse_scores(txt):
    #we want to split on '\t' and '\n' only. Here's how:
    txt = txt.replace('\t', '\n')
    txt = txt.split('\n')
    games = []
    teams = []
    for i in range(len(txt)/6):
        winner = txt[(i*6)+5].strip().lower()
        if winner == txt[(i*6)+2].strip().lower():
            loser = txt[(i*6)+3].strip().lower()
            w_num = 0
        else:
            loser = txt[(i*6)+2].strip().lower()
            w_num = 2
        winner = team_exists(winner, teams)
        loser = team_exists(loser, teams)
        score = txt[(i*6)+4].split()
        win_score = int(score[w_num])
        lose_score = int(score[2-w_num])
        assert win_score > lose_score
        d = txt[(i*6)].lower().split()
        d = mx.DateTime.Date(int(d[2]), nmonth(d[0]), int(d[1]))
        games.append(Game(winner, loser, win_score, lose_score, d))
        winner.game_list.append(games[-1])
        loser.game_list.append(games[-1])
    return (games, teams)

def find_team(name, teams):
    for t in teams:
        if t.name == name: return t

def do_ratings(games, teams):
    debug = file('debug.out', 'w')
    for g in games:
        #calculate winning expectancies
        wt_we = 1/(1 + 10 ** ((g.loser.rating - g.winner.rating) / 400))
        lt_we = 1 - wt_we
        print >>debug, '----------------------------------------------'
        print >>debug, "%s(%d) %d - %s(%d) %d: %f" % \
            (g.winner.name, g.winner.rating, g.wpoints, g.loser.name, 
            g.loser.rating, g.lpoints, wt_we)
        g.winner.update_rating(g.gp, wt_we)
        g.loser.update_rating(g.gp_l, lt_we)
        print >>debug, "%s(%d)    %s(%d)" % \
            (g.winner.name, g.winner.rating, g.loser.name, g.loser.rating)

def calc_sos(games, teams):
    """Calculate the strength of each team's schedule"""
    for g in games:
        g.winner.opp_rating += g.loser.rating
        g.loser.opp_rating += g.winner.rating
        g.winner.games += 1
        g.loser.games += 1
    for t in teams: t.sos = t.opp_rating / t.games

def calc_change(games, teams, date):
    """calculate a teams' ranking change since date"""
    for t in teams:
        count = 1
        for g in t.game_list:
            if g.date > date: count += 1
        t.change = t.rating - t.all_ratings[-count]

def average_scores(games):
    #calculate average scores
    totall = 0
    totalw = 0
    for g in games:
        totall += g.lpoints
        totalw += g.wpoints
    print "average loss points: ", totall/float(len(games))
    print "average win points:  ", totalw/float(len(games))

def sig_predictor(we): 
    #this is the sigmoidal function I chose for predicting scores
    #based on winning expectancy (see wt_we in do_ratings)
    return 18 / (1 + math.exp(-9 * (we - .5))) - 9

def sig_predictor2(we): 
    #this is the sigmoidal function I chose for predicting scores
    #based on winning expectancy (see wt_we in do_ratings)
    val = 18 / (1 + math.exp(-9 * (we - .5))) - 9
    if round(val) > 4: return 5
    else: return val

def lin_predictor(we):
    #a linear predictor, just to see what happens
    return 2*(9*(we - .5))

def exp_predictor(we):
    #an exponential predictor
    return 70*((we-.5)**3)

def find_rating(team, date):
    """find the team's rating *before* date"""
    count = 1
    for g in team.game_list:
        if g.date >= date: count += 1   #count all games today and later
    return team.all_ratings[-count]     #then return the rating before them

def calc_predictions(games, predictor):
    wrong = 0
    plotpoints = []
    predictpoints = []
    for game in games:
        if game.date > mx.DateTime.Date(2004, 7, 11):
            rdiff = find_rating(game.winner, game.date) - \
                find_rating(game.loser, game.date)
            we = 1 - (1 / (1 + 10 ** (rdiff/400.)))
            prediction = predictor(we)
            plotpoints.append((we, game.pd))
            predictpoints.append((we, prediction))
            #sig_p = sig_predictor(we)
            #lin_p = lin_predictor(we)
            diff = game.pd
            #####
            # test this function
            #####
            #test_w = find_rating(game.winner, game.date)
            #test_l = find_rating(game.loser, game.date)
            #print "%4d vs %4d sig: %2d lin: %2d actual: %d" % \
            #    (test_w, test_l, round(sig_p), round(lin_p), diff)
            #end test
            wrong += abs(round(prediction) - diff)
            #print "actual: %d predicted: %.1f" % (diff, prediction)
    x = Gnuplot.Gnuplot()
    x.plot(plotpoints, predictpoints)
    z = raw_input('sheeit')
    print "total wrong: %d (%s)" % (wrong, predictor.__name__)

def calc_future(txt, teams, predictor):
    txt = txt.replace('\t', '\n').split('\n')
    for i in range(len(txt)/6):
        teama = team_exists(txt[(i*6)+2].strip().lower(), teams)
        teamb = team_exists(txt[(i*6)+3].strip().lower(), teams)
        if teama.rating > teamb.rating: winner, loser = (teama, teamb)
        else:                           winner, loser = (teamb, teama)
        rdiff = winner.rating - loser.rating
        we = 1 - (1 / (1 + 10 ** (rdiff/400.)))
        pd = predictor(we)
        print "%9s (%4d) over %9s (%4d) by %d points" % (winner.name, \
            round(winner.rating), loser.name, round(loser.rating), round(pd))

def print_results(teams):
    summ = [(t.rating, t.name, t.change) for t in teams]
    summ.sort()
    summ.reverse()
    print "   %-10s %10s %10s" % ("Captain", "Rating", "Change")
    count = 0
    for x in summ:
        count += 1
        print "%2d %-10s %10.4f %+6.1f" % \
            (count, x[1], x[0], x[2])

if __name__ == "__main__":
    f = file('scores.dat')
    games, teams = parse_scores(f.read())
    do_ratings(games, teams)
    calc_sos(games, teams)
    week_ago = mx.DateTime.now() + mx.DateTime.RelativeDate(days=-6)
    calc_change(games, teams, week_ago)
    print_results(teams)
    #calc_predictions(games, sig_predictor)
    #calc_predictions(games, sig_predictor2)
    #calc_predictions(games, lin_predictor)
    #calc_predictions(games, exp_predictor)
