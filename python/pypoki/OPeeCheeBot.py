"""This is a sample poker bot based on the PyPokiBot class. It plays only
premium hands, but these very aggresively

Why the name? A company called OPeeChee made a "Premium" series of baseball 
cards in the early 90s. They were nice cards."""

from PyPokiBot import PyPokiBot, FOLD, CALL, RAISE
import time
import sys

class OPeeCheeBot(PyPokiBot):
    def __init__(self):
        PyPokiBot.__init__(self, 'OPCBot', 'premium', 'OPCBot')
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
            self.log("""I have $%d and have won %d%% of the hands I've played
this session (%d%% bb/100)""" % (me.money_won, win_pct, bb_per_100))
        else:
            print "I'm not in right now"
    
    def getbet(self):
        #need a better way to express "AA-QQ: raise; AKs-AQs,KQs: call" etc.
        hs = self.heval
        ranks = self.hand[0][0] + self.hand[1][0]
        if hs.hand == []:
            hs.evaluate_starting_hand(self.hand)
        elif self.board != hs.board:
            hs.evaluate_board(self.board)
        if hs.pairs and ('A' in hs.pairs or 'K' in hs.pairs or 'Q' in hs.pairs):
            return RAISE
        if ranks == 'AK' or (ranks == 'AQ' and hs.suited) or \
            (ranks == 'KQ' and hs.suited):
            return CALL
        return FOLD

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

    def evaluate_starting_hand(self, hand):
        self.log('evaluating hand %s' % hand)
        self.hand = hand
        rank1, suit1 = hand[0]
        rank2, suit2 = hand[1]
        if rank1 == rank2:
            self.log('pair of %s' % rank1)
            self.pairs = [rank1]
        elif suit1 == suit2:
            self.log('suited %s' % suit1)
            self.suited = [suit1]

    def evaluate_board(self, board):
        self.log('evaluating board %s' % board)
        self.board = board
        #for c in (board[0], board[1]):

    def log(self, msg):
        print >> self.logfile, time.strftime('%m-%d-%y %H:%M: '), msg

if __name__ == "__main__":
    opcb = OPeeCheeBot()
