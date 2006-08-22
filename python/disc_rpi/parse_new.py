from elo_new import Team, Game, do_sauceda
import pylab as g

def parse_data(fin):
    teams = {}
    games = []
    for line in fin:
        if not line.strip(): continue
        try:
            dt, field, team1, team2, score, winner = \
                [x.strip() for x in line.split('\t')]
            score_l, score_w = \
                sorted([int(x) for x in score.strip().split('-')])
            #why do I need to strip this? (otherwise strptime fails)
            dt = dt.strip()
        except ValueError:
            print "Could not decode line:\n%s" % line
            raise
        if winner == team2:
            loser = team1
        elif winner == team1:
            loser = team2
        else:
            #it's a tie; pick a name for each
            winner = team1
            loser = team2
        teams[winner] = teams.get(winner, Team(winner))
        teams[loser] = teams.get(loser, Team(loser))
        games.append(Game(teams[winner], score_w, teams[loser], score_l, dt))
        if score_w > score_l:
            teams[winner].wins += 1
            teams[loser].losses += 1
        else:
            teams[winner].draws += 1
            teams[loser].draws += 1
    return teams, games

def test_value(K):
    teams, games = parse_data(file('scores.dat'))
    do_sauceda(games, K)
    for game in games:
        pass

def test_we(K):
    teams, games = parse_data(file('scores.dat'))
    do_sauceda(games, K)
    r_strata = [(0., 0., 0) for i in range(50)] #pd, rd, n of games
    for game in games:
        #is this calc correct? I'm not sure
        w_rank = int(round(game.winner.history[game.w_idx]))
        l_rank = int(round(game.loser.history[game.l_idx]))
        if w_rank < l_rank: pd = -game.pd
        else:               pd = game.pd
        r = w_rank, l_rank
        rd = max(r) - min(r)
        temp_pd, temp_rd, temp_n = r_strata[rd / 50]
        r_strata[rd / 50] = (temp_pd + pd, temp_rd + rd, temp_n + 1)
    for i, (pd, rd, n) in enumerate(r_strata):
        if n > 0:
            print "%3d-%3d: %f %d %f (%d)" % \
                (i * 50, (i * 50) + 50, pd / rd, pd, rd, n)
    pd_tot = sum(r[0] for r in r_strata)
    rd_tot = sum(r[1] for r in r_strata)
    print "total:   %f (%d)" % (pd_tot/rd_tot, rd_tot)

def test_K():
    tot_correct = []
    K_range = range(10, 401, 10)
    for K in K_range:
        teams, games = parse_data(file('scores.dat'))
        do_sauceda(games, K)
        correct = 0.
        incorrect = 0.
        even = 0
        for game in games:
            if game.w_idx < 6 or game.l_idx < 6: continue
            w_rank = round(game.winner.history[game.w_idx], 0)
            l_rank = round(game.loser.history[game.l_idx], 0)
            if w_rank > l_rank: correct += 1
            elif w_rank < l_rank: incorrect += 1
            else: even += 1
        #print "------------\nK at %d" % K
        #print "Percent correct: %f%% Even: %d %d" % (correct/len(games), even, correct)
        tot_correct.append(correct/len(games))
    g.plot(K_range, tot_correct, '-b')
    g.show()

if __name__=='__main__':
    test_K()
    #test_we(200)
