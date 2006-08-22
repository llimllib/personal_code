#!/usr/bin/env python
#import numarray as na
from random import Random
from __future__ import generators

"""a card is represented as a string of the form "{rank}{suit}" where rank is
a number from 1-10 or one of "A", "K", "Q", "J", and a suit is one of "S", 
"C", "D", or "H". Thus, the ace
of spades is "AS" and the king of clubs is "KC"."""

class Cards:
    def __init__(self):
        #we'll start with a simple card representation, and remember
        #that it may change
        self.ranks = [str(x + 2) for x in range(8)]
        self.ranks.extend(['A', 'K', 'Q', 'J'])
        self.suits = ["S", "C", "D", "H"]
        self.deck = [x + y for x in self.ranks for y in self.suits]
        self.rand = Random()

    def reset(self):
        self.ranks = [x for x in range(13)]
        self.suits = ["S", "C", "D", "H"]
        self.deck = [str(x) + y for x in self.ranks for y in self.suits]

    def deal(self, n):
        """generate, remove, and return n cards from the deck"""
        hand = []
        cards = []
        for i in range(n):
            hand.append(self.pop_card(self.rand.randint(0,len(self.deck)-1)))
        return hand

    def pick_card(self, n):
        """generate a hand randomly, but don't remove the cards"""
        hand = []
        cards = []
        for i in range(n):
            card = self.rand.randint(0,len(self.deck)-1)
            if card not in cards:
                cards.append(card)
        for card in cards:
            hand.append(self.deck[card])
        return hand

    def deal_card(self, card):
        """
        remove a card from the deck - accepts the card string ("AC", "10S")
        """
        try: self.pop_card(self.deck.remove(card))
        except ValueError: raise "Card is not in deck"

    def pop_card(self, idx):
        """remove an index from the deck and return the card"""
        try: card = self.deck[idx]
        except IndexError: return ''
        self.deck.remove(card)
        return card

    def gen_hand(self, n):
        if n > len(deck): raise "too many cards"
        counters = range(n)

    def randint(self, x, y):
        return self.rand.randint(x,y)
