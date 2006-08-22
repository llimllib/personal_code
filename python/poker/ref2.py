#!/usr/bin/python
from cards import Cards
from players import *       #used in text_ui
import commands, sys

try: import readline
except: pass

class HoldEmRef:
    """the successor to the PokerRef class."""
    def __init__(self):
        """initialize all variables"""
        self.dealer = -1
        self.deck = Cards()
        self.players = []       #this array contains only players, not viewers
        self.n_play = len(self.players)  #number of current players
        self.cur_player = None  #the player on whom the action rests
        self.min_bet = 0        #the current minimum a player must bet
        self.pot = 0            #the current value of the pot
        self.no_raise = 1       #true if the hand has not yet been raised
        self.calls = 0          #number of current calls


    ###
    #public functions
    ###

    def set_players(self, players):
        """players should be an array of types derived from PokerPlayer

        These should be people sitting at the table, not viewers of the game"""
        self.players = players
        self.n_play = len(self.players)

    def rem_player(self, p):
        """remove a player from the game"""

    def get_blinds(self, ante, blind):
        """get the blinds and ante from the players"""
        self._init_hand()
        if ante:
            for p in self.players:
                p.payAnte(ante)
                self.transmit_bet(p, ante)
        sb = int(round(blind/2.0))
        small = self.next_active(self.dealer)
        self.big_blind = self.next_active(small)
        if not small.payBlind(sb): raise "blind error"
        if not self.big_blind.payBlind(blind): raise "blind error"
        self.transmit_bet(small, sb)
        self.transmit_bet(self.big_blind, blind)
        self.min_bet = blind
        self.cur_player = self.next_active(self.big_blind)

    def get_next_bet(self):
        """get the next player's bet"""
        self.cur_player.transmitBet(self.min_bet - self.cur_player.active_bet)

    def deal(self):
        """deal the cards to the players"""
        for p in self.players: p.re_init()
        cards = self.deck.deal(self.n_play*2)
        if self.dealer == -1:
            #pick a random player
            self.dealer = self.players[self.deck.randint(0, self.n_play-1)]
        else: self.dealer = self.next_active(self.dealer)
        cur_player = self.next_active(self.dealer)
        for card in cards:
            cur_player.addCard(card)
            cur_player = self.next_active(cur_player)

    ###
    #private functions
    ###

    def _init_hand(self):
        """reinitialize the values needed for the hand"""
        self.n_play = len(self.players)
        self.cur_player = None
        self.min_bet = 0
        self.pot = 0
        self.no_raise = 1
        self.calls = 0
        for p in self.players: p.active = 1
        self.active_players = self.n_play

    def count_active(self):
        count = 0
        for p in self.players:
            if p.active: count += 1
        return count

    def next_active(self, p):
        """return the next to play after p"""
        if not type(3) == type(p): p = p.position
        p = self.players[(p+1) % self.n_play]
        while not p.active: p = self.players[(p+1) % self.n_play]
        return p

    def transmit_bet(self, player, bet):
        for p in self.players:
            p.receiveBet(bet, p.position)

    def receive_bet(self, bet, player):
        p = self.players[player]
        p.active_bet += bet
        if p.active_bet < self.min_bet and bet != -1:
            p.active_bet -= bet
            raise BetTooSmallError
        if p.active_bet > p.money:
            p.active_bet -= bet
            raise BetTooBigError
        if bet == -1: self._fold(p)
        elif p.active_bet == self.min_bet: self._call(p, bet)
        else: self._raise(p, bet)
        self.transmit_bet(p, bet)
        self.cur_player = self.next_active(self.cur_player)

    def _fold(self, player):
        player.active_bet = 0
        player.active = 0
        self.active_players -= 1
        if not self.active_players: self.endHand()

    def _call(self, player, bet):
        """handle a player's call"""
        self.pot += player.active_bet
        player.money -= bet
        self.calls += 1
        if self.calls >= self.active_players and not self._big_blind_exempt():
            if len(self.comm_cards) == 0: self.deal_comm_cards(3)
            elif len(self.comm_cards) == 3: self.deal_comm_cards(1)
            elif len(self.comm_cards) == 4: self.deal_comm_cards(1)
            else: self.endHand()
        else: self.get_next_bet()

    def deal_comm_cards(self, n): pass
    
    def _big_blind_exempt(self):
        """return true if this is the special case where the big blind gets to
        raise his bet. We already know calls > active_players"""
        if len(self.comm_cards) == 0\
            and self.cur_player.position == self.big_blind.position\
            and self.no_raise:
            return 1
        return 0
            
    def _raise(self, player, bet):
        """handle a player's raise"""
        self.min_bet = player.active_bet
        self.pot += bet
        player.money -= bet
        self.calls = 0
        self.no_raise = 0

class text_ui:
    """a text ui to the hold 'em ref"""
    def __init__(self):
        """initialize all variables"""
        self.cash = 0
        self.players = []     #the players array
        self.me = None        #reference to the human-controlled player
        self.blind = 0        #size of the big blinds
        self.ante = 0

    def main(self):
        self.ref = HoldEmRef()
        self.get_init_values()
        self.players = self.get_players()
        self.ref.set_players(self.players)
        self.get_blinds()
        self.deal()
        self.get_bet()

    def get_bet(self):
        self.ref.get_next_bet()

    def get_human_bet(self, min):
        q = "How much would you like to bet? (minimum %d) [%d]" % (min, min)
        bet = self.get_int(q, min, self.me.money, default=min)

    def get_blinds(self):
        self.ref.get_blinds(self.ante, self.blind)

    def deal(self):
        self.ref.deal()
        if self.me:
            self.print_cards(self.me.getHand())
        else: print "robot game"
        
    def get_init_values(self):
        q = "How much money would you like each player to play with? [10000] "
        self.cash = self.get_int(q, 1, 10000000, default=10000)
        q = "How big should the big blinds start? [100] "
        self.blind = self.get_int(q, 1, 1000000, default=100)
        q = "How big should the ante be? [0] "
        self.ante = self.get_int(q, 1, 1000000, default=0)
        #other init values?

    def get_players(self):
        players = []
        q = "How many total players? [10]"
        n = self.get_int(q, 2, 10, default=10)
        #eventually, we'll need a list of available types of computer
        #players here
        q = "Which player is human? (0 for none) [1]"
        human = self.get_int(q, 0, n, default=1)
        for i in range(n):
            if i + 1 != human:
                players.append(PokerBot(self.cash, 'bot%d'%i, i, self.ref))
            else: 
                name = raw_input("What is the human player's name? ")
                if not name: name = "Joe Smith"
                players.append(PokerHuman(self.cash, name, i, self))
                self.me = players[i]
        return players

    ###
    # utility functions
    ###

    def print_cards(self, cards):
        """pretty print a card array. @cards must be an array."""
        out = sys.stdout
        for card in cards:
            rank = int(card[:-1])
            suit = card[-1:]
            if 0 < rank < 10: out.write(str(rank + 1))
            elif rank == 0: out.write("A")
            elif rank == 12: out.write("K")
            elif rank == 11: out.write("Q")
            elif rank == 10: out.write("J")
            out.write(suit + " ")
        out.write('\n')

    def get_int(self, text, min, max, **kwargs):
        """return an integer from stdin.

        keywords:
        err: message to print on error
        default: number to return if user enters ''"""
        try:
            n = raw_input(text)
            if n == '' and kwargs.has_key('default'): return kwargs['default']
            n = int(n)
            if n < min or n > max: raise ValueError
            print
            return n
        except ValueError:
            if kwargs.has_key('err'): print kwargs['err']
            else: print "Please enter a number between %s and %s\n" % \
                (min, max)
            return self.get_int(text, min, max, err)

if __name__ == "__main__":
    x = text_ui()
    x.main()
