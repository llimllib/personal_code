#!/usr/bin/python
import wx, cards
from wxPython.lib import colourdb
from ref import PokerRef, BetTooSmallError, BetTooBigError

"""
TODO: 
o Hand decision program work on windows?
o New Game Dialog
    o Change player's names
    o Change player's types
    o Change betting structure (? - no limit only? 5/10?)
    o Change starting bankrolls
    o Good hands only? (advanced)
o Different players <fun> <fun> :p
o Config files saving/loading stuff. What goes in there?
o Save games
o Load games
o Pretty it up?
    o Why doesn't the background color work?
    o Get player's names centered over their cards
    o Make sure this will be easy to do; keep it in mind
    o put dealer button in good spots
"""

_EVT_GET_BET = wx.NewEventType()
EVT_GET_BET = wx.PyEventBinder(_EVT_GET_BET, 1)

class BetEvent(wx.PyCommandEvent):
    def __init__(self, evtType, id):
        wx.PyCommandEvent.__init__(self, evtType, id)

class PokerFrame(wx.Frame):
    def __init__(self, parent):
        ########################
        ### Class-wide variables
        ########################
        self.title = "Bill Mill Poker v.01"
        self.players = []
        self.player_labels = []
        self.money_labels = []
        self.card_refs = []
        self.comm_cards = []
        self.card_back = wx.Image("pics/deck1.png", wx.BITMAP_TYPE_PNG)\
            .ConvertToBitmap()
        self.button = wx.Image("pics/button.png", wx.BITMAP_TYPE_PNG)\
            .ConvertToBitmap()
        self.static_button = None
        self.dealer = -1
        self.pot_label = None
        self.small_blind = 5
        self.big_blind = 10
        self.ref = PokerRef(self)
        self.positions = [[40, 200], [150, 10], [300, 10], [450, 10], [600, 10],
                    [680, 200], [600, 520], [450, 520], [300, 520], [150, 520]]
        self.card_pos = [[10, 240], [120, 50], [270, 50], [420, 50], [570, 50],
                    [650, 240], [570, 400], [420, 400], [270, 400], [120, 400]]
        self.comm_card_pos = [270, 240] #cur pos of community cards
        self.table_size = 10         #max number of players at table
        self.bet_but = None          #bet, check and fold buttons
        self.check_but = None
        self.fold_but = None
        
        ##################
        ### Frame setup
        ##################
        wx.Frame.__init__(self, parent, -1, self.title, size=(760,580),
                style=wx.DEFAULT_FRAME_STYLE | wx.NO_FULL_REPAINT_ON_RESIZE)
        self.Bind(EVT_GET_BET, self.OnBetEvent, self)
        self.Centre()
        self.makeMenu()
        #set a background color? Why this no work!!!
        #self.SetBackgroundColour()
        self.ClearBackground()

    def clearTable(self, winner, pot, players):
        """clear the table after a hand has been played"""
        upcards = []
        for card in range(len(self.card_refs)-5): 
            self.card_refs.pop(0).Destroy()
        for player in players:
            hand = player.getHand()
            cur_pos = player.position
            pos = self.card_pos[cur_pos]
            card_img = self.cardToImgname(hand[0])
            card_img = wx.StaticBitmap(self, -1, card_img, pos)
            card_img2 = self.cardToImgname(hand[1])
            card_img2 = wx.StaticBitmap(self, -1, card_img2, \
                (pos[0] + 20, pos[1]))
            upcards.append(card_img)
            upcards.append(card_img2)
        if len(winner) == 1:
            dlg2 = wx.MessageDialog(self, 
                'Player %s wins %d dollars' % (winner[0], pot),
                'Hand Over', wx.OK | wx.ICON_INFORMATION)
            dlg2.ShowModal()
            dlg2.Destroy()
        else:
            dlg2 = wx.MessageDialog(self, 
                'Players %s win %d dollars each' % (winner, pot/len(winner)),
                'Hand Over', wx.OK | wx.ICON_INFORMATION)
            dlg2.ShowModal()
            dlg2.Destroy()
        for card in upcards: card.Destroy()
        for card in self.card_refs: card.Destroy()
        self.card_refs = []
        self.static_button.Destroy()
        self.refreshPot(0)
        self.comm_card_pos = [270, 240]

    def makeMenu(self):
        self.mainmenu = wx.MenuBar()
        
        menu = wx.Menu()
        id = wx.NewId()
        menu.Append(id, "&New Game")
        wx.EVT_MENU(self, id, self.onNewGame)
        id = wx.NewId()
        menu.Append(id, "E&xit")
        wx.EVT_MENU(self, id, self.onExit)
        id = wx.NewId()
        
        self.mainmenu.Append(menu, "&File")
        self.SetMenuBar(self.mainmenu)

    def onNewGame(self, e):
        #TODO: new game dialog
        self.num_players = 10
        #make all players human for now
        p = ['b' for i in range(self.table_size)]
        p[0] = 'h'
        self.ref.newGame(p, 25, 0)

    def onExit(self, e):
        self.Close()

    def fireBetEvent(self):
        print "firing bet"
        e = BetEvent(_EVT_GET_BET, self.GetId())
        self.GetEventHandler().ProcessEvent(e)

    def OnBetEvent(self, e):
        self.ref.getNextBet()

    def getBet(self, player, min_bet):
        self.cur_player = player
        self.min_bet = min_bet
        print "gb: %d" % player
        bet_id = wx.NewId()
        check_id = wx.NewId()
        fold_id = wx.NewId()
        pos = [200, 370]
        self.bet_but = wx.Button(self, bet_id, "bet", pos, (100, 20))
        pos[0] += 120
        if min_bet == 0:
            self.check_but = wx.Button(self, check_id, "check", pos, (100,20))
            wx.EVT_BUTTON(self, check_id, self.OnCheck)
        else:
            self.check_but = wx.Button(self, check_id, "call", pos, (100,20))
            wx.EVT_BUTTON(self, check_id, self.OnCall)
        pos[0] += 120
        self.fold_but = wx.Button(self, fold_id, "fold", pos, (100, 20))
        wx.EVT_BUTTON(self, bet_id, self.OnBet)
        wx.EVT_BUTTON(self, fold_id, self.OnFold)
        
    def DestroyBetButtons(self):
        self.bet_but.Destroy()
        self.check_but.Destroy()
        self.fold_but.Destroy()

    def OnCheck(self, e):
        print "oncheck"
        try:
            self.DestroyBetButtons()
            self.ref.receiveBet(0, self.cur_player)
        except BetTooSmallError:
            dlg2 = wx.MessageDialog(self, 
                'You must bet at least the big blind',
                'Error', wx.OK | wx.ICON_INFORMATION)
            dlg2.ShowModal()
            dlg2.Destroy()
            self.getBet(self.cur_player, self.min_bet)

    def OnCall(self, e):
        print "oncall"
        self.DestroyBetButtons()
        self.ref.receiveBet(self.min_bet, self.cur_player)

    def OnFold(self, e):
        print "onfold"
        self.DestroyBetButtons()
        self.ref.receiveBet(-1, self.cur_player)

    def OnBet(self, e):
        print "onbet"
        dlg = wx.TextEntryDialog(self, 'What is your bet (in whole numbers)?',
                            'Bet:', 'Bill Mill Poker')
        dlg.SetValue("")
        if dlg.ShowModal() == wx.ID_OK:
            try:
                self.DestroyBetButtons()
                self.ref.receiveBet(int(dlg.GetValue()), self.cur_player)
            except ValueError:
                self.getBet(self.cur_player, self.min_bet)
            except BetTooSmallError:
                dlg2 = wx.MessageDialog(self, 
                    'You must bet at least the small blind',
                    'Error', wx.OK | wx.ICON_INFORMATION)
                dlg2.ShowModal()
                dlg2.Destroy()
                self.getBet(self.cur_player, self.min_bet)
            except BetTooBigError:
                dlg2 = wx.MessageDialog(self,
                    'You do not have enough money to place this bet',
                    'Error', wx.OK | wx.ICON_INFORMATION)
                dlg2.ShowModal()
                dlg2.Destroy()
                self.getBet(self.cur_player, self.min_bet)
        dlg.Destroy()

    def dealCommCards(self, cards):
        self.min_bet = 0
        for card in cards:
            card_img = self.cardToImgname(card)
            card_img = wx.StaticBitmap(self, -1, card_img, self.comm_card_pos)
            self.card_refs.append(card_img)
            self.comm_card_pos[0] += 40

    def turn(self, card):
        #TODO: most of this should be abstracted into a function
        pos = [310, 240]
        card_img = self.cardToImgname(card)
        card_img = wx.StaticBitmap(self, -1, card_img, pos)
        self.card_refs.append(card_img)

    def river(self, card):
        pos = [350, 240]
        card_img = self.cardToImgname(card)
        card_img = wx.StaticBitmap(self, -1, card_img, pos)
        self.card_refs.append(card_img)

    def refreshPot(self, pot):
        self.pot_label.SetLabel("Pot: $%s" % pot)

    def refreshMoney(self, players):
        """refresh the money label for each player"""
        for p in players:
            self.money_labels[p.position].SetLabel(str(p.money))

    def setUpPlayers(self, p):
        """initialize n players"""
        self.num_players = len(p)
        for i in range(self.table_size):
            if p[i]:
                pos = self.positions[i]
                #TODO: look at the wxStaticText demo for how to work the 
                #centering
                self.player_labels.append(\
                    wx.StaticText(self, -1, p[i].name, pos))
                if pos[1] < 500: pos[1] += 20
                else: pos[1] -= 20
                self.money_labels.append(\
                    wx.StaticText(self, -1, "$%s" % p[i].money, pos))
            else:
                self.player_labels.append(None)
                self.money_labels.append(None)
        self.pot_label = wx.StaticText(self, -1, "Pot: $0", (150, 270))
        self.pot_label.SetFont(wx.Font(18, wx.SWISS, wx.NORMAL, wx.NORMAL))
    
    def deal(self, p, dealer):
        """Deal cards to the players"""
        count = 1
        button_pos = self.card_pos[p[dealer].position][:]
        button_pos[0] -= 35
        self.static_button = wx.StaticBitmap(self, -1, self.button, button_pos)
        for player in p:
            hand = player.getHand()
            cur_pos = player.position
            pos = self.card_pos[cur_pos]
            if player.type == "HUMAN":
                card_img = self.cardToImgname(hand[0])
                card_img = wx.StaticBitmap(self, -1, card_img, pos)
                card_img2 = self.cardToImgname(hand[1])
                card_img2 = wx.StaticBitmap(self, -1, card_img2, \
                    (pos[0] + 20, pos[1]))
            else:
                card_img = wx.StaticBitmap(self, -1, self.card_back, pos)
                card_img2 = wx.StaticBitmap(self, -1, self.card_back, \
                    (pos[0] + 20, pos[1]))
            self.card_refs.append(card_img)
            self.card_refs.append(card_img2)
            count += 1

    def clearHand(self, player):
        cards = (self.card_refs[player * 2], self.card_refs[player * 2 + 1])
        for card in cards:
            card.Hide()

    def cardToImgname(self, card):
        """given a card, return the filename of the appropriate card"""
        rank = (12 - int(card[:-1])) * 4
        suit = card[-1:]
        if suit == "C": suit = 1
        elif suit == "S": suit = 2
        elif suit == "H": suit = 3
        elif suit == "D": suit = 4
        card = rank + suit
        card_img = wx.Image("pics/%s.png" % card, \
            wx.BITMAP_TYPE_PNG).ConvertToBitmap()
        return card_img

class PokerApp(wx.App):
    def OnInit(self):
        """
        Start the game
        """
        #import locale
        #self.locale = wx.Locale(wx.LANGUAGE_FRENCH)
        #locale.setlocale(locale.LC_ALL, 'fr')
        wx.Image_AddHandler(wx.PNGHandler())
        frame = PokerFrame(None)
        frame.Show(True)
        self.SetTopWindow(frame)
        #wx.InitAllImageHandlers()
        return True

def main():
    app = PokerApp(wx.Platform == "__WXMAC__")
    app.MainLoop()

if __name__ == "__main__":
    main()
