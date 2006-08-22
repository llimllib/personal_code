#!/usr/bin/python
"""This class provides a simple, extensible interface to Poki Poker Servers

The university of Alberta has created a free poker server, where bots may play
via the Online Poker Protocol 
(http://games.cs.ualberta.ca/webgames/poker/bots.html). This class implements
the protocol.

This code should work on both Linux and Windows (at least with cygwin). To use 
it, create a subclass of the PyPokiBot class. You must implement the getbet 
method, so the simplest poker bot looks like:

from PyPokiBot import PyPokiBot, FOLD, CALL, RAISE

class Samplebot(PyPokiBot):
    def __init__(self):
    PyPokiBot.__init__(self, 'MyBotUser', 'MyBotPass', 'MyBotName')
    PyPokiBot.login(self)

    def getbet(self):
        return FOLD    #always fold

Hopefully, you will want to write a more interesting getbet() function. For
a slightly more complicated example, look at OPeeCheeBot.py.

This software is currently fairly stable, but WAY beta."""

#
#By Bill Mill bill.mill@gmail.com
#
#
#Thanks to Aaron Davidson for the protocol, for Poki, and for his work on
#computer poker. (thanks too to the other U of A people involved - excellent 
#work)
#Thanks to the guys who wrote Ethereal. Your program rocks! Without it, this
#would have been a much more difficult program to write.
#
#this code is public domain. copy it, change it, sell it, just don't expect me
#to come running if it doesn't work or breaks something. If you like it or
#have questions, drop me an email.
#
#THIS CODE COMES WITH NO WARRANTY

import socket
import time
import sys

#note: there should be a "huh?" message in this protocol; how should the server
#respond to unknown or ill-formed messages?
#What if a bot just wants to observe? What would have to be added for that?

#please don't import * - all this sh*t will corrupt your namespace.
#Really, it's a bad practice anyway. Get over it.
FOLD = 0
CALL = 1
RAISE = 2
BLIND = 3

JOIN_GAME = 20
GOODPASS = 21
BADPASS = 22
ILLNICK = 23    # I think 23 means "illegal nickname" - too long, bad chars
BADNICK = 24

ACTION = 30
CHAT = 32
QUIT_GAME = 33

GET_GRAPH = 42
INFORMATION = 43
SET_FACE = 45
GET_GACE = 46
CHANGE_FACE = 47

START_NEW_GAME = 50
HOLE_CARDS = 51
NEW_STAGE = 52
WINNERS = 53
CHATTER = 54
NEXT_TO_ACT = 57

PING = 60
PONG = 61

class PokerPlayer:
    """Contains statistics about your opponents

    You can access this info through the PyPokiBot.players list, but please
    do not change any of the values.

    This has not yet been tested; I'm not sure it works."""
    def __init__(self, position, name, cash, face):
        self.position = position
        self.name = name
        self.cash = cash
        self.face = face
        self.active = 1
        self.hands_played = 0
        self.flops_seen = 0
        self.turns_seen = 0
        self.rivers_seen = 0
        self.calls = 0
        self.raises = 0
        self.folds = 0
        self.wins = 0
        self.money_won = 0

class PyPokiBot:
    """Class which communicates with Online Poker Protocol servers

    Please see module documentation (above) for details"""
    server = 'hilo.cs.ualberta.ca'
    port = 55006   #Bots only test room
    pokisocket = (server, port)

    def __init__(self, uid, pwd, name):
        self.uid = uid
        self.pwd = pwd
        self.name = name

        #game status data
        self.bet_size = 0   #size of the raise amount
        self.cur_bet = 0    #size of total bet this round
        self.pot = 0        #total size of pot
        self.n_players = 0
        self.n_active = 0
        self.active = 0
        self.button = 0
        self.position = 0
        self.gameid = 0
        self.hand = []
        self.players = []
        self.board = []
        self.me = None      #reference to my player in self.players

        #dictionary mapping message => function to handle message
        self.actions = {
            GOODPASS: self.game_loop,
            BADPASS: self.uname_error,
            ILLNICK: self.uname_error,
            BADNICK: self.uname_error,
            CHATTER: self.print_chatter,
            START_NEW_GAME: self.start_new_game,
            INFORMATION: self.parse_info,
            HOLE_CARDS: self.hole_cards,
            NEW_STAGE: self.new_stage,
            NEXT_TO_ACT: self.next_to_act,
            FOLD: self.receive_fold,
            CALL: self.receive_call,
            RAISE: self.receive_raise,
            BLIND: self.receive_blind,
            PING: self.pong,
            WINNERS: self.winners
        }

        self.sock = socket.socket()
        self.sock.connect(self.pokisocket)

    def getbet(self):
        """This function needs to be implemented in a subclass"""
        raise NotImplementedError

    def login(self):
        """Log player in and start the main message loop"""
        v1 = self.ltos(1)
        msg = "%s\0%s\0%s%s\0" % (self.uid, self.pwd, v1, self.name)
        self.sock.send(self.ltos(JOIN_GAME))
        self.sock.send(self.ltos(len(msg)))
        self.sock.sendall(msg)
        self.log("sent message #%d len %d" % (JOIN_GAME, len(msg)))
        res = self.recvlng()
        self.log('received message %d' % res)
        self.actions[res]()

    #####################
    # MESSAGE FUNCTIONS

    def uname_error(self): raise "There was an error logging onto Poki server"

    def game_loop(self):
        """Start the main message loop

        This function should only be called by login()"""
        zero = self.recvlng()
        self.log('succesful login')
        while 1:
            res = self.recvlng()
            #self.log('received message %d' % res)
            self.msglen = self.recvlng()
            try:
                self.actions[res]()
            except KeyError:
                raise "No handler for action %d" % res

    def print_chatter(self): print self.recvmsg(self.msglen)

    def parse_info(self):
        #doesn't parse info yet while you're watching a game - not sure if I
        #want it to. If you do, write it!
        msg = self.recvmsg(self.msglen)
        self.log(msg[:-1])
        print msg

    def start_new_game(self):
        self.bet_size = self.recvlng()
        self.n_players = self.recvlng()
        self.n_active = self.n_players
        self.active = 1
        self.button = self.recvlng()
        self.position = self.recvlng()
        self.gameid = self.recvlng()
        msg = self.recvmsg(self.msglen - 20)
        pos = 0
        while msg:
            x = msg.find('\0')
            name = msg[:x]
            msg = msg[x+1:]
            cash = self.stol(msg[:4])
            face = self.stol(msg[4:8])
            msg = msg[8:]
            try:
                p = self.players[pos]
                if p.name == name:
                    self.log('found player %s' % name)
                    p.cash = cash
                    p.face = face
                    p.active = 1
                    p.hands_played += 1
                else: raise IndexError
            except IndexError:
                self.log('added player %s' % name)
                self.players.append(PokerPlayer(pos, name, cash, face))
            if self.name == name: self.me = self.players[pos]
            pos += 1
        self.log('players are: %s' % str([p.name for p in self.players]))

    def hole_cards(self):
        pos = self.recvlng()
        cards = self.recvmsg(6)[:5].split(' ') #:5 cuts off the /0 ending
        if pos == self.position:
            self.hand = cards
            print 'received cards: %s' % cards
            self.log('received cards: %s' % cards)
        else:
            print 'player %s showed: %s' % (self.players[pos].name, cards)
            self.log('player %s showed: %s' % (self.players[pos].name, cards))

    def new_stage(self):
        round = self.recvlng()
        cards = self.recvmsg(self.msglen-4)[:-1].split(' ')
        self.cur_bet = 0
        if round > 0:
            for card in cards:
                if card not in self.board:
                    self.board.append(card)
            if round == 1:
                for p in self.players:
                    if p.active: p.flops_seen += 1
            if round == 2:
                for p in self.players:
                    if p.active: p.turns_seen += 1
            if round == 3:
                for p in self.players:
                    if p.active: p.rivers_seen += 1
        else: self.board = []
        self.log('new round %d with board %s' % (round, self.board))

    def next_to_act(self):
        pos = self.recvlng()
        tocall = self.recvlng()
        minbet = self.recvlng()
        maxbet = self.recvlng()
        if pos == self.position:
            bet = self.getbet()
            msg = self.ltos(ACTION) + self.ltos(4) + self.ltos(bet)
            self.sock.sendall(msg)
            if not bet: self.log('I folded')
            elif bet == 1: self.log('I called')
            else: self.log('I raised. booyeah!')
        self.log('player #%d next to act' % pos)

    def receive_fold(self):
        if self.active:
            who = self.recvlng()
            if self.msglen == 8: zero = self.recvmsg(4)
            self.players[who].active = 0
            self.players[who].folds += 1
            self.n_active -= 1
            self.log('player %s (#%d) folded' % (self.players[who].name, who))

    def receive_raise(self):
        if self.active:
            who = self.recvlng()
            amt = self.recvlng()
            self.pot += amt
            self.cur_bet += amt
            self.players[who].raises += 1
            self.log('player %s (#%d) raised $%d' % \
                (self.players[who].name, who, amt))

    def receive_call(self):
        if self.active:
            who = self.recvlng()
            amt = self.recvlng()
            self.pot += amt
            self.players[who].calls += 1
            self.log('player %s (#%d) called $%d' % \
                (self.players[who].name, who, amt))

    def receive_blind(self):
        if self.active:
            who = self.recvlng()
            amt = self.recvlng()
            self.pot += amt
            if amt > self.cur_bet: self.cur_bet = amt
            self.log('player %s (#%d) blinded $%d' % \
                (self.players[who].name, who, amt))

    def winners(self):
        """I'm not certain that this function works for more than 1 winner; it's
        yet to be tested"""
        n_winners = self.recvlng()
        for i in range(n_winners):
            who = self.recvlng()
            cash = self.recvlng()   #don't add to cash; reset at start_new_game
            p = self.players[who]
            p.wins += 1
            p.money_won += cash
            self.log('player %s won %d' % (p.name, cash))
            self.log('%s has won %d games $%d' % (p.name, p.wins, p.money_won))

    def quit(self):
        self.log('quit game')
        msg = self.ltos(QUIT_GAME) + self.ltos(0)
        self.sock.sendall(msg)

    def pong(self):
        self.log('ponged')
        msg = self.ltos(PONG) + self.ltos(0)
        self.sock.sendall(msg)

    def chat(self, msg):
        msg = self.ltos(CHAT) + self.ltos(len(msg) + 1) + msg + '\0'
        self.sock.sendall(msg)

    #######################
    # UTILITY FUNCTIONS

    def log(self, msg):
        """log a message"""
        if not self.logfile:
            self.logfile = file('pypoki.out', 'w')
        print >> self.logfile, time.strftime('%m-%d-%y %H:%M: '), msg

    def stol(self, s):
        """convert a string into a long"""
        assert len(s) == 4
        n = 0L
        for i in range(4):
            n += (256 ** (3-i)) * ord(s[i])
        return n

    def ltos(self, n):
        """convert a long into a string"""
        s = '' 
        for x in (2**24, 2**16, 2**8):
            quot, rem = (n/x, n%x)
            s += chr(quot)
            n = rem
        s += chr(n % 256)
        return s
   
    def hexdump(self, msg, out = sys.stdin):
        """print a hexdump of msg"""
        count = 0
        for c in msg:
            if count % 8 == 0: print >> out
            elif count % 4 == 0: print >> out, '  ',
            print >> out, '0x%-2x' % ord(c),
            count += 1
        print >> out, '\nend msg'

    def recvmsg(self, bytes):
        """get a msg from the poki server of length bytes"""
        b = ''
        while len(b) < bytes:
            chunk = self.sock.recv(bytes - len(b))
            if chunk == '': raise RuntimeError
            b += chunk
        return b

    def recvlng(self):
        """get a long from the poki server"""
        b = ''
        #how to find the length of the message? Unclear from packet capture
        while len(b) < 4:
            chunk = self.sock.recv(4 - len(b))
            if chunk == '': raise RuntimeError
            b += chunk
        return self.stol(b)

if __name__ == '__main__':
    #p = PyPokiBot('pytestbot', 'pytestbot', 'pytestbot')
    #needs to be subclassed to run right now
    raise "You need to subclass PyPokiBot and implement the getbet method"
