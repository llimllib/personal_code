#!/usr/bin/python
"""Aardvark bot is an expert poker player

I wrote it because I thought I should start my bot-writing with an expert
player. If it's any good, I can have future bots play against it to see if
they're any good.

Why the name? 2002 WSOP Main Event - Geek poker player Robert Varkonyi wins
a million bucks.
"""

from PyPokiBot import PyPokiBot, FOLD, CALL, RAISE
import time
import sys
import random

class AardvarkBot(PyPokiBot):
    def __init__(self):
        PyPokiBot.__init__(self, 'AardvarkBot', 'Varkonyi', 'AardvarkBot')
        self.logfile = open('opcbot.log', 'w')
        self.heval = HoldemHandEvaluator()
        try:
            PyPokiBot.login(self)
        except KeyboardInterrupt:
            PyPokiBot.quit(self)

    def start_new_game(self):
        """print statistics out before starting a new game"""
        PyPokiBot.start_new_game(self)
        if self.me:
            me = self.me
            try:
                win_pct = me.flops_seen / float(me.wins)
            except ZeroDivisionError:
                win_pct = 0
            #is the bb always 2*sb?
            bb_per_100 = me.money_won / float(self.bet_size * 2)
            self.log("cash: $%d wins: %d%% (%d bb/100)" % \
                (me.money_won, win_pct, bb_per_100))
        else:
            print "I'm not in right now"
    
    def getbet(self):
        #need a better way to express "AA-QQ: raise; AKs-AQs,KQs: call" etc.
        heval = self.heval
        ranks = self.hand[0][0] + self.hand[1][0]
        if heval.hand == []:
            heval.evaluate_starting_hand(self.hand)
        elif self.board != heval.board:
            heval.evaluate_board(self.board)
        evaluate_strategy()
        self.strategy()
    
    #############
    # Strategies

    def strat_allin(self): return RAISE

    def strat_allcall(self): return CALL

    def strat_allfold(self): return FOLD

    def strat_callsmall(self):
        if self.cur_bet < 3 * self.bet_size: return CALL
        return FOLD

    def strat_smallraise(self):
        if self.cur_bet < 2 * self.bet_size: return RAISE
        return CALL

    #############
    # Strategy Choosers

    def evaluate_strategy(self):
        if self.board == []: self.strategy = eval_preflop()
        elif len(self.board) == 3: self.strategy = eval_flop()
        #in future, change strategy postturn and postriver

    def eval_preflop():
        heval = self.heval
        die = random.random()
        
        #Given a big pair, let's raise no questions asked
        if heval.pairs[0] > 9: return self.strat_allin
        
        #On a middle pair, let's call a small raise to see if we hit it on the
        #flop; up to (but not including) 3 BB
        if 5 < heval.pairs[0] < 10: return self.strat_callsmall

        #On A-K or A-Q, try to get a small raise out of people
        if heval.total_rank > 21: return self.strat_smallraise

        #half the time, play medium suited connectors
        if die > .5 and heval.suited and \
            abs(heval.hand[0].rank - heval.hand[1].rank) < 2:
            return self.strat_allcall

        else return self.strat_fold

class HoldemHandEvaluator:
    def __init__(self):
        self.logfile = file('eval.out', 'w')
        self.reset()

    def reset(self):
        self.pairs = []
        self.suited = []
        self.flush = []
        self.flush_draw = 0
        self.sets = []
        self.straight = 0
        self.gutshot_draw = 0
        self.outside_draw = 0
        self.hand = []
        self.board = []
        self.total_rank = 0

    def evaluate_starting_hand(self, hand):
        self.log('evaluating hand %s' % hand)
        cards = [card(c) for c in hand]
        self.total_rank = cards[0].rank + cards[1].rank
        if cards[0] == cards[1]:
            self.log('pair of %s' % rank1)
            self.pairs = [cards[0].rank]
        elif cards[0].suit == cards[1].suit:
            self.log('suited %s' % suit1)
            self.suited = [cards[0].suit]

    def evaluate_board(self, board):
        self.log('evaluating board %s' % board)
        new_cards = [card(c) for c in board if c not in self.board]
        for c in new_cards: self.board.append(c)
        for i in range(len(board)):
            for j in range(i+1, len(board)):
                if board[0]

    def log(self, msg):
        print >> self.logfile, time.strftime('%m-%d-%y %H:%M: '), msg

class card:
    def __init__(self, card):
        if card[0] == 'A': self.rank = 12
        elif card[0] == 'K': self.rank = 11
        elif card[0] == 'Q': self.rank = 10
        elif card[0] == 'J': self.rank = 9
        elif card[0] == 'T': self.rank = 8
        else: self.rank = int(card[0]) - 2
        self.suit = card[1]

    def __gt__(self, other):
        if self.rank > other.rank: return 1
        return 0

    def __ge__(self, other):
        if self.rank >= other.rank: return 1
        return 0

if __name__ == "__main__":
    avb = AardvarkBot()
