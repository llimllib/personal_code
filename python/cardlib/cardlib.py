#!/usr/bin/python
"""very basic card library. Cards are represented by a string of rank,suit
such as "10h", "0s", "12C", "8d". 0 = Two, 1 = Three, ... , 11 = King,
12 = Ace

to create one or more cards, simply create an instance of the stack class
with the desired number of cards.

things to add: card highlighting, draggable cards, stacks where some cards
                are face up and not others"""
import wx

class stack:
    """represents a stack of one or more cards.

    panel:  the panel on which to draw the cards
    origin: the point from which to start drawing
    cards:  an array of card strings, or a single card string. See cardlib
            documentation for information on these.
    sep:    a tuple or list [x,y] describing the separation between cards
    faceup: true if the cards are faceup, false otherwise"""
    def __init__(self, panel, origin, cards, sep=[0,0], faceup=1, size=None):
        self.panel = panel
        self.origin = origin
        self.point = list(origin)     #current card location
        self.size = size
        if type(cards) == type(''):
            self.cards = [card(cards, faceup, size)]
        else:
            self.cards = [card(c, faceup, size) for c in cards]
        self.sep = sep
        self.faceup = faceup
        self.put_cards()

    def put_cards(self):
        for c in self.cards:
            c.bmp = wx.StaticBitmap(self.panel, -1, c.im, self.point)
            #move point by an increment of sep
            self.point = [a+b for a,b in zip(self.point, self.sep)]
        self.point = self.origin #reset point

    def clear(self):
        for c in self.cards: c.Destroy()

    def flip(self):
        self.clear()
        self.faceup = (self.faceup + 1) % 2
        #this is probably not the best way to do it; design stack->card
        #interface better? Should card be a public object?
        self.cards = [card(c.card, self.faceup) for c in self.cards]
        self.put_cards()

class card:
    """card class just for the use of stack"""
    def __init__(self, card, faceup, size = None):
        self.card = card
        self.card_back = wx.Image("deck1.png", wx.BITMAP_TYPE_PNG)
        if size: self.card_back.Rescale(size[0], size[1])
        if faceup: self.im = self.card_to_imgname(card)
        else:      self.im = self.card_back
        if size: self.im.Rescale(size[0], size[1])
        self.im.ConvertToBitmap()
        self.bmp = None       #gets created by stack.put_cards
        self.size = size

    def Destroy(self): 
        if self.bmp: self.bmp.Destroy()

    def card_to_imgname(self, card):
        """given a card, return the filename of the appropriate card"""
        rank = (12 - int(card[:-1])) * 4
        suit = card[-1:]
        if suit == "C": suit = 1
        elif suit == "S": suit = 2
        elif suit == "H": suit = 3
        elif suit == "D": suit = 4
        card = rank + suit
        return wx.Image("%s.png" % card, wx.BITMAP_TYPE_PNG)
