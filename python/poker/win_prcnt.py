"""
Doubts, thoughts:
o We can calculate, with 5 cards left, how many hands add a 3rd or a 3rd 
  and a 4th to a pocket pair
o We can calculate how many hands add a pair to a non-pair

Things we need to know:
o how many possible hands there are to be dealt
o how many of those have >2 hearts
o how many of those give either hand a straight
o how many of those have >0 A's
o how many of those have a 9 and an 8

-- How many possible hands there are --
o 48 choose 5 = (48*47*46*45*44)/ 5! = 1,712,304

LATER:
read source code for poker evaluator in C.
"""
from cards import *

#we'll use these two hands for testing
hand1 = ["12s", "12c"]
hand2 = ["9h", "8h"]
hands = [hand1, hand2]

def win_percent(deck, hands):
    #assumes cards are valid
    for hand in hands:
        for card in cards:
            deck.deal_card(card)
    
def cond_prob(top, bot):
    top_sum = 1
    fact = 1
    for i in range(bot):
        top_sum *= top - i
        fact *= i + 1
    return top_sum/float(fact)

def test_prob(deck):
    ace_count = 0.
    try:
        deck.deck.remove("12S")
        deck.deck.remove("12H")
    except ValueError: pass
    iter = 100000
    n_cards = 3
    for i in range(iter):
        h = deck.pick_card(n_cards)
        for card in h:
            if card[:2] == "12":
                ace_count += 1
                break
    print (ace_count/iter) * 100, " percent"

#test_prob(cards())
