#####################################################################
# Calculates sauceda rankings, whose definition can be found at
# http://www.mratings.com/theory/sauceda.htm
#####################################################################

import time, os
from datetime import date

#K = 40        #rate of convergence (as defined in sauceda)
PDV = 4.5     #point differential value

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
        self.rri = 100
        
    def update_ranking(self, gp, we, game, K):
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
        return self.ranking - self.history[-g]

class Game:
    def __init__(self, winner, score_w, loser, score_l, dt):
        self.winner = winner
        self.score_w = score_w
        self.loser = loser
        self.score_l = score_l
        self.dt = date(*time.strptime(dt, '%b %d %Y %I:%M%p')[:3])
        
        self.pd = score_w - score_l

def we(pd):
    return 1 - .4 ** (1 + pd)

def we2(pd):
    return 1

def we3(pd):
    return 1 - .4 ** (1 + (pd / 4.5))

def do_sauceda(games, K):
    for game in games:
        ###############
        # each team splits one game point by this formula, where pd is point
        # differential
        ###############
        if game.pd == 0:
            wgp = lgp = .5
        else:
            wgp = we3(game.pd)
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
        game.loser.update_ranking(lgp, lwe, game, K)
        game.winner.update_ranking(wgp, wwe, game, K)

        game.l_idx = len(game.loser.history) - 1
        game.w_idx = len(game.loser.history) - 1
