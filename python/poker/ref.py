#!/bin/env python
"""PokerRef referees a poker game

Current Problems:
o Big blind doesn't get a chance to check
o What to do about blinds in general? What do you do when they don't accept?
"""

from cards import Cards
from players import *
import commands, wx

class BetTooSmallError: pass

class BetTooBigError: pass

class PokerRef:
    def __init__(self, gui):
        self.dealer = -1
        self.players = []
        self.gui = gui
        self.deck = Cards()
        self.pot = 0
        self.cur_player = None
        self.min_bet = 0
        self.calls = 0
        self.active_hands = 0
        self.comm_cards = []        #community cards
        self.no_raise = 1           #set to false if somebody has raised the
                                    #big blind
        self.big_blind = 0          #denotes the big blind
        self.hand_over = 0          #true if the hand is over

    def mainLoop(self):
        while 1:
            self.deal()
            self.cur_player = (self.dealer + 1) % len(self.players)
            while not self.hand_over:
                self.getNextBet(self.nextActive(self.cur_player))
                self.gui.Update()
                #assure that the bet has been received before proceeding
            self.hand_over = 0

    def nextActive(self, cur):
        cur = (cur + 1) % len(self.players)
        while self.players[cur].active == 0:
            cur = (cur + 1) % len(self.players)
        return cur

    def countActive(self):
        count = 0
        for p in self.players:
            if p.active: count += 1
        return count

    def newGame(self, players, blind, ante):
        """start a new game

        players is an array with a string. 'h' means human player, for now.
        blind is the size of the small blind. The big will be assumed to be
            twice that.
        ante is the size of the ante.
        """
        self.players = players
        self.blind = blind
        self.ante = ante
        for i in range(len(players)):
            if players[i] == 'h': 
                players[i] = PokerHuman(10000, "player%s" % i, i)
            elif players[i] == 'b': 
                players[i] = PokerBot(10000, "player%s" % i, i, self)
            elif players[i] == None: pass
            else: raise "illegal player"
        self.gui.setUpPlayers(players)
        self.dealer = -1
        self.deal()

    def cardsToCmpName(self, cards):
        outCards = []
        for card in cards:
            b = int(card[:-1]) + 2
            if b > 9:
                if b == 10:
                    b = 't'
                elif b == 11:
                    b = 'j'
                elif b == 12:
                    b = 'q'
                elif b == 13:
                    b = 'k'
                elif b == 14:
                    b = 'a'
            outCards.append(str(b) + card[-1:])
        return outCards

    def getWinner(self):
        self.winner = []
        cmd = './mycmpn '
        self.eval_hands = []
        positions = []
        for p in self.players:
            if p.active:
                positions.append(p.position)
                for card in self.cardsToCmpName(p.getHand()):
                    cmd += '%s ' % card
        cmd += '-- '
        for card in self.cardsToCmpName(self.comm_cards):
            cmd += '%s ' % card
        output = commands.getoutput(cmd).split()
        print cmd, "\n", output
        if len(output) == 1:
            win_pos = positions[int(output[0])]
            self.players[win_pos].money += self.pot
            self.winner.append(win_pos)
        else:
            split = int(self.pot) / len(output)
            for i in output:
                win_pos = positions[int(i)]
                self.players[win_pos].money += split
                self.winner.append(win_pos)
        return self.winner
 
    def endHand(self):
        """code to run at the end of a hand"""
        print "ending hand"
        self.winner = []
        if self.active_hands == 1:
            self.players[self.nextActive(0)].money += self.pot
            self.winner.append(self.nextActive(0))
        else: self.winner = self.getWinner()
        self.gui.clearTable(self.winner, self.pot, self.players)
        self.deck.reset()
        for p in self.players: p.endHand(self.winner)
        self.pot = 0
        self.cur_player = self.dealer
        self.min_bet = 0
        self.calls = 0
        self.active_hands = 0
        self.no_raise = 1
        self.comm_cards = []
        self.gui.refreshMoney(self.players)
        self.hand_over = 0
        self.deal()

    def deal(self):
        """deal 2 cards to each player"""
        cards = self.deck.deal(len(self.players)*2)
        if self.dealer == -1:
            self.dealer = 8
            #self.dealer = self.deck.randint(0, len(self.players)-1)
        else: self.dealer = (self.dealer + 1) % len(self.players)
        count = 1
        for card in cards:
            cur_player = (self.dealer + count) % len(self.players)
            self.players[cur_player].addCard(card)
            count += 1
        self.getBlinds()
        self.gui.deal(self.players, self.dealer)
        self.active_hands = self.countActive()
        self.gui.refreshPot(self.pot)
        self.cur_player = (self.dealer + 2) % len(self.players)
        self.gui.fireBetEvent()

    def getBlinds(self):
        small_blind = (self.dealer + 1) % len(self.players)
        self.big_blind = (small_blind + 1) % len(self.players)
        for p in self.players:
            if p.position == small_blind:
                if p.payBlind(self.blind): 
                    p.receiveBet(self.big_blind, self.blind * 2)
                else: raise "what to do if they don't accept the blind?"
            elif p.position == self.big_blind:
                if p.payBlind(self.blind * 2): 
                    p.receiveBet(small_blind, self.blind)
                else: raise "what to do if they don't accept the blind?"
            else:
                p.receiveBet(small_blind, self.blind)
                p.receiveBet(self.big_blind, self.blind * 2)
        self.gui.refreshMoney(self.players)
        self.pot += self.blind * 3
        self.min_bet = self.blind * 2

    def dealCommCards(self, num):
        """Deal num community cards"""
        print "dealing %d community cards" % num
        self.min_bet = 0
        self.calls = 0
        cards = self.deck.deal(num)
        for card in cards: self.comm_cards.append(card)
        self.gui.dealCommCards(cards)
        for p in self.players:
            p.receiveCommCards(cards)
            p.active_bet = 0

    def fold(self, p):
        """folds a hand.
        
        p should be an actual reference to the player"""
        print "player %d folds" % p.position
        p.active = 0
        self.gui.clearHand(p.position)
        self.active_hands -= 1
        self.transmitBet(-1, p.position)
        self.gui.refreshMoney(self.players)
        self.gui.refreshPot(self.pot)
        if self.active_hands == 0 or self.active_hands - 1 == self.calls:
            self.hand_over = 1
            self.endHand()
        else: self.gui.fireBetEvent()

    def raiseBet(self, bet, p):
        """performs a raise.
        
        bet should be a bet amount
        p should be a reference to a player"""
        print "player %d raises to $%d" % (p.position, bet)
        self.pot += bet
        p.money -= bet
        self.calls = 0
        self.no_raise = 0
        self.min_bet = p.active_bet
        self.transmitBet(bet, p.position)
        self.gui.refreshMoney(self.players)
        self.gui.refreshPot(self.pot)
        self.gui.fireBetEvent()

    def callBet(self, bet, p):
        """performs a call.

        bet should be a bet amount
        p should be a reference to a player"""
        print "player %d calls for $%d. Active: %d. no_raise: %d" % \
            (p.position, bet, p.active_bet, self.no_raise)
        self.pot += bet
        p.money -= bet
        self.calls += 1
        self.transmitBet(bet, p.position)
        self.gui.refreshMoney(self.players)
        self.gui.refreshPot(self.pot)
        #the second condition allows the big blind to bet on the first round
        #if nobody raises to him
        print self.calls, self.active_hands, self.hand_over
        if self.calls >= self.active_hands and not\
            self.isBigBlindBet(self.nextActive(self.cur_player)):
            self.cur_player = self.nextActive(self.dealer)
            if len(self.comm_cards) == 0: 
                self.no_raise = 0           #big blind can only raise 1st round
                self.dealCommCards(3)
            elif len(self.comm_cards) == 3: self.dealCommCards(1)
            elif len(self.comm_cards) == 4: self.dealCommCards(1)
            elif len(self.comm_cards) == 5: 
                self.hand_over = 1
                self.endHand()
        if not self.hand_over: self.gui.fireBetEvent()

    def isBigBlindBet(self, p):
        """returns true if it's big blind's chance to bet on the first round
        
        p is the number of the next player"""
        if p == self.big_blind and self.no_raise and len(self.comm_cards) == 0:
            return 1
        return 0

    def getNextBet(self):
        player = self.nextActive(self.cur_player)
        print "getNextBet from %d" % player
        if self.players[player].type == "HUMAN":
            self.gui.getBet(player, self.min_bet - \
                self.players[player].active_bet)
        else:
            self.players[player].transmitBet(self.min_bet - \
                self.players[player].active_bet)

    def receiveBet(self, bet, player):
        self.cur_player = player
        p = self.players[player]
        if bet > 0: p.active_bet += bet
        if bet < 0: self.fold(p)                #this call has side effects!
        elif p.active_bet < self.min_bet:
            p.active_bet -= bet
            raise BetTooSmallError
        elif p.active_bet > p.money:
            p.active_bet -= bet
            raise BetTooBigError
        elif p.active_bet == self.min_bet: self.callBet(bet, p)
        elif p.active and p.active_bet > self.min_bet:
            self.raiseBet(bet, p)
        else: raise "Something is fscked up!!1!"

    def transmitBet(self, bet, player):
        """transmits a bet to all current players

        bet should be the dollar amount of the bet (<0 for folds)
        player should be the player's position on the table"""
        for p in self.players:
            p.receiveBet(bet, player)
