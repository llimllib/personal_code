#I'll probably want to serialize each player and save it somewhere. Cool new
#stuff. Use pickle class?
class PokerPlayer:
    def __init__(self, cash, name, position):
        self.money = cash
        self._hand = []
        self.name = name
        self.position = position    #seat num at the table
        self.blind = 0
        self.active = 1             #1 if player is currently not folded
        self.active_bet = 0         #money currently in open pot
        self.comm_cards = []        #community cards
        self.pot = 0

    def re_init(self):
        self._hand = []
        self.blind = 0
        self.active = 1             #1 if player is currently not folded
        self.active_bet = 0         #money currently in open pot
        self.comm_cards = []        #community cards
        self.pot = 0

    def endHand(self, winner):
        self.clearHand()
        self.active=1

    def addCard(self, card):
        self._hand.append(card)

    def clearHand(self):
        self._hand = []

    def getHand(self):
        return self._hand

    def payBlind(self, blind):
        """return 1 if you want to pay the blind, 0 else"""
        self.money -= blind
        self.active_bet += blind
        self.pot += blind
        return 1

    def payAnte(self, ante):
        self.money -= blind
        self.pot += ante

    def transmitBet(self):
        """return bet amt

        return -1 to fold or the bet amount
        """
        pass

    def receiveBet(self, amt, player):
        """get bet from another player"""
        self.pot += amt

    def receiveCommCards(self, cards):
        for card in cards: self.comm_cards.append(card)

class PokerHuman(PokerPlayer):
    def __init__(self, cash, name, pos, ui):
        PokerPlayer.__init__(self, cash, name, pos)
        self.type = "HUMAN"
        self.ui = ui

    def transmit_bet(self, min):
        self.ui.get_human_bet(min)

class PokerBot(PokerPlayer):
    def __init__(self, cash, name, position, ref):
        PokerPlayer.__init__(self, cash, name, position)
        self.type = "BOT"
        self.ref = ref
        self.log = open("%dbot" % position, 'w')

    def transmitBet(self, min):
        print >>self.log, "bot %d bets min of %d. active_bet: %d" \
            % (self.position, min, self.active_bet)
        if min > self.money: self.ref.receiveBet(self.money, self.position)
        else: self.ref.receive_bet(min, self.position)
