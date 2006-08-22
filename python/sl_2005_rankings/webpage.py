from elo import do_sauceda, Game, Team
from datetime import date
import Image

FIN = file('scores.dat')

def parse_data(fin):
    teams = {}
    games = []
    for line in fin:
        if not line.strip(): continue
        try:
            dt, field, team1, team2, score, winner = \
                [x.strip() for x in line.split('\t')]
            if score == 'N/A': 
                continue #rain out
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

def parse_future(fin):
    games = []
    for line in fin:
        if not line.strip(): continue
        dt, loc, t1, t2, _, _ = [x.strip() for x in line.strip().split('\t')]
        games.append((t1, t2))
    return games

def print_future(teams, games, fout):
    print >>fout, "<h2>Next Week's Predictions:</h2>"
    print >>fout, """<table cellpadding="3"><tr><td><b>Winner</b></td><td><b>Loser</b></td><td><b>Margin</b></td></tr>"""
    for t1, t2 in games:
        r1 = teams[t1].ranking
        r2 = teams[t2].ranking
        diff = abs(r1 - r2)
        if r1 > r2:
            w, l = t1, t2
        else:
            l, w = t1, t2
        print >>fout, "<tr><td>%s</td><td>%s</td><td>%.1f</td></tr>" \
            % (w, l, diff * .02)

def make_rosters(fin):
    rosters = {}
    blank = 1
    for line in fin:
        line = line.strip()
        if not line:
            blank = 1
            continue
        line = line.strip('"')
        line = line.replace('""', '"')
        if blank:
            try:
                team, n, captain = line.split('-')
            except ValueError:
                team, n = line.split('-')
                captain = team
            f = file(team + '.txt', 'w')
            blank = 0
        else:
            f.write(line + '\n')

def print_rankings(teams, games, fout):
    print >>fout, """\
<html><head><title>CT Ultimate Summer League Rankings</title></head><body>
<table cellpadding="3"><tr><td>Team Name</td><td>Ranking</td><td>Record</td>
<td>Pt Diff</td><td>Relative</td><td>Week Change</td><td>SOS (rank)</td>
<td>Roster</td></tr>"""
    max_rank = max([team.ranking for team in teams])
    min_rank = min([team.ranking for team in teams])
    max_diff = max_rank - min_rank
    max_dt = max([g.dt for g in games])
    seconds_in_day = 24*60*60
    soses = list(sorted([(t.calc_sos(), t.name) for t in teams]))
    soses = dict([(t[1], (i, t[0])) for i, t in enumerate(reversed(soses))])
    for i, team in enumerate(teams):
        rank_pct = int(((team.ranking - min_rank) / max_diff) * 100)
        bar = Image.new("RGB", (rank_pct + 2, 10), (0, 0, 255))
        change = int(round(team.week_change(max_dt)))
        if change > 0:
            change = '<font color="green">%+d</font>' % change
        elif change < 0:
            change = '<font color="red">%+d</font>' % change
        else:
            change = ' 0'
        bar.save('team%d.gif' % i)
        #Sean's team contains illegal file name chars (real solution: sanitize)
        if team.name.find('Sean') > -1:
            roster = 'Sean Laing'
        else:
            roster = team.name
        sos = "%.1f (%d)" % (soses[team.name][1], soses[team.name][0] + 1)
        rank = str(i + 1).rjust(2, ' ').replace(' ', '&nbsp;')
        s = '''\
<tr><td>%s. <a href="%s.png">%s</a></td><td>%d</td><td>%d-%d-%d</td><td>%d</td>
<td><img src="team%d.gif"></td><td>%s</td><td>%s</td><td>
<a href="%s.txt">Roster</a>
</td></tr>''' % \
        (rank, roster, team.name, team.ranking, team.wins, \
        team.losses, team.draws, team.pd, i, change, sos, roster)
        print >>fout, s
    print >>fout, "</table>"

def graph_teams(teams):
    import pylab as g
    rankings = []
    for i, team in enumerate(reversed(teams)):
        rankings = team.history + [team.ranking]
        #rankings.append(team.ranking)
        #g.text(i+.5, team.ranking-6, team.name)
        for j, game in enumerate(team.games):
            if game.winner.name == team.name:
                opp = game.loser.name
            else:
                opp = game.winner.name
            g.text(j, rankings[j], opp)
        g.plot(range(len(rankings)), rankings, 's-b')
        if team.name.find('Sean') > -1:
            name = 'Sean Laing'
        else:
            name = team.name
        g.savefig('%s.png' % name)
        g.clf()
        #g.show()

def get_ranking(team): return team.ranking

if __name__ == '__main__':
    teams, games = parse_data(FIN)
    do_sauceda(games)
    make_rosters(file('sl2005rosters.dat'))
    #sort teams by ranking
    ordered = list(reversed(sorted(teams.values(), key=get_ranking)))
    #and print them out
    fout = file('elo.html', 'w')
    print_rankings(ordered, games, fout)
    #now graph them
    graph_teams(list(ordered))
    #future_games = parse_future(file('future.dat'))
    #print_future(teams, future_games, fout)
    print >> fout, "</body></html>"
