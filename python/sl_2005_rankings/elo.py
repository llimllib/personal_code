#####################################################################
# Calculates sauceda rankings, whose definition can be found at
# http://www.mratings.com/theory/sauceda.htm
#####################################################################

import time, os
from datetime import date

K = 60        #rate of convergence (as defined in sauceda)
PDV_9 = 4.5     #point differential value for games to 9
PDV_15 = 7.5    #point diff value for games to 15

#what we have here is really a many-to-2 database relationship between teams
#and games - implementing in db right now
class Team:
    def __init__(self, name):
        self.ranking = 1000
        self.name = name
        self.history = []
        self.games = []
        self.wins = 0
        self.losses = 0
        self.draws = 0
        self.pd = 0
        self.sos = 0
        
    def update_ranking(self, gp, we, game):
        self.history.append(self.ranking)
        self.games.append(game)
        self.ranking = self.ranking + (K * (gp - we))
        if gp > .5:
            self.pd += game.pd
        else:
            self.pd -= game.pd

    def week_change(self, max_game_dt):
        """determine the change in ranking in the week before max_game_dt
        
        max_game_dt should be a datetime.date object"""
        g = len([g for g in self.games if (max_game_dt - g.dt).days < 7])
        if g > 0:
            return self.ranking - self.history[-g]
        return 0

    def calc_sos(self):
        """Calculate strength of schedule"""
        for game in self.games:
            if game.winner.name == self.name:
                self.sos += game.loser.ranking
            elif game.loser.name == self.name:
                self.sos += game.winner.ranking
            else:
                print game.winner.name, game.loser.name, self.name
                raise "name not matched in team.sos"
        return self.sos / float(len(self.games))

class Game:
    def __init__(self, winner, score_w, loser, score_l, dt):
        self.winner = winner
        self.score_w = score_w
        self.loser = loser
        self.score_l = score_l
        self.dt = date(*time.strptime(dt, '%b %d %Y %I:%M%p')[:3])
        
        self.pd = score_w - score_l

def do_sauceda(games):
    for game in games:
        ###############
        # each team splits one game point by this formula, where pd is point
        # differential
        ###############
        if game.pd == 0:
            wgp = lgp = .5
        else:
            #for games before Jul 25 (when they switch to 15-pointers:
            cutoff = date(2005, 7, 24)
            if game.dt < cutoff:
                wgp = 1 - .4 ** (1 + (game.pd / PDV_9))
            else:
                wgp = 1 - .4 ** (1 + (game.pd / PDV_15))
            lgp = 1 - wgp

        ###############
        # winning expectancy is defined for each team as follows (Elo's
        # formula)
        ###############
        wwe = 1 / (1 + 10 ** ((game.loser.ranking - game.winner.ranking) / 400))
        lwe = 1 - wwe

        ###############
        # and now we can update each team's ranking (see update_ranking)
        ###############
        game.loser.update_ranking(lgp, lwe, game)
        game.winner.update_ranking(wgp, wwe, game)
