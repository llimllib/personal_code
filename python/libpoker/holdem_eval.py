try:
    doesntexist
    import psyco
    psyco.full()
except: pass

ranks = {'A': 12, 'K': 11, 'Q': 10, 'J': 9}
for i in range(2, 11):
    ranks[str(i)] = i-2
suits = {'S': 0, 'C': 1, 'H': 2, 'D': 3}

try:
    sorted
except NameError:
    def sorted(seq):
        seq.sort()
        return seq

def make_cards(cards):
    """turn cards in "10H" format into (8, 2)"""
    #I don't have python 2.4, so no genexp
    return sorted([(ranks[c[:-1].upper()], suits[c[-1].upper()]) for c in cards])

HIGH_CARD = 0
PAIR = 1
TWO_PAIR = 2
SET = 3
STRAIGHT = 4
FLUSH = 5
FULL_HOUSE = 6
FOUR_OF_A_KIND = 7
STRAIGHT_FLUSH = 8

def holdem_eval(board, hands):
    #maxhand format:
    #hand value, comparable, best hand, hand desc
    #hand value is the value of hand; 8 for 4 of a kind, 1 for pair, etc
    #comparable is any value able to compare between two hands of the same value
    #best hand is a 2-tuple of the best hand
    #hand desc is a textual desc of the hand
    maxhand = (-1, -1, (), '')

    for hand in hands:
        straight = -2
        straights = []
        n_sf = 0
        suit_sf = []
        str_flushes = []
        flushes = [0, 0, 0, 0]
        highflush = [0, 0, 0, 0]
        pairs = [0 for i in range(13)]
        cards = make_cards(board + hand)
        #card loop
        for card in cards:
            rank, suit = card
            pairs[rank] += 1
            flushes[suit] += 1
            highflush[suit] = rank
            if rank - straight == 1:
                straight += 1
                if suit not in suit_sf:
                    suit_sf = [suit]
                    n_sf = 0
                else:
                    n_sf += 1
            elif rank - straight > 1:
                straight = 0
                suit_sf = []
                n_sf = 0
            else:
                suit_sf.append(suit)
            if straight > 3:
                straights.append(card)
                if n_sf > 3:
                    str_flushes.append(card)
        if str_flushes:
            hand = (STRAIGHT_FLUSH, str_flushes[-1], hand, "Straight Flush")
            if hand > maxhand:
                maxhand = hand
            continue
        try:
            rank = pairs.index(4)
            hand = (FOUR_OF_A_KIND, rank, hand, "Four of a kind")
            if hand > maxhand:
                maxhand = hand
            continue
        except ValueError: pass
        sets = []
        pairs_ = []
        if FULL_HOUSE < maxhand: continue
        for rank, qty in enumerate(pairs):
            if qty == 3:
                sets.append(rank)
            elif qty == 2:
                pairs_.append(rank)
        if sets and pairs_:
            hand = (FULL_HOUSE, sets[-1], hand, "Full House")
            if hand > maxhand:
                maxhand = hand
            continue
        f = [i for i in range(len(flushes)) if flushes[i] > 4]
        #now we're going to need high cards to distinguish hands
        cards.reverse()
        if FLUSH < maxhand: continue
        if f:
            suit = max([highflush[i] for i in f])
            flush = [c for c in cards if c[1] == suit][:5]
            hand = (FLUSH, flush, hand, "Flush")
            if hand > maxhand:
                maxhand = hand
            continue
        if STRAIGHT < maxhand: continue
        if straights:
            hand = (STRAIGHT, straights[-1], hand, "Straight")
            if hand > maxhand:
                maxhand = hand
            continue
        if SET < maxhand: continue
        if sets:
            hand = (SET, [sets[-1], cards[0], cards[1]], hand, "Set")
            if hand > maxhand:
                maxhand = hand
            continue
        if TWO_PAIR < maxhand: continue
        if len(pairs_) > 1:
            hand = (TWO_PAIR, [pairs_[-1], pairs_[-2], cards[0]], hand,
                    "Two Pair")
            if hand > maxhand:
                maxhand = hand
            continue
        if PAIR < maxhand: continue
        if pairs_:
            hand = (PAIR, [pairs_[-1], cards[0], cards[1], cards[2]], hand, 
                    "Pair")
            if hand > maxhand:
                maxhand = hand
            continue
        hand = (HIGH_CARD, cards[:5], hand, "High Card")
        if hand > maxhand:
            maxhand = hand
    return maxhand

def xuniqueCombinations2(items, n):
    if n==0: yield []
    else:
        for i in xrange(len(items)-n+1):
            for cc in xuniqueCombinations2(items[i+1:],n-1):
                yield [items[i]]+cc

if __name__ == "__main__":
    #run test
    deck = [x+y for x in ranks.iterkeys() for y in suits.iterkeys()]
    hand1 = ["AC", "8C"]
    hand2 = ["KC", "6C"]
    board = ["9S", "4D", "6H", '', '']
    one, two = 0, 0
    for board in xuniqueCombinations2(deck, 5):
        #board[3], board[4] = board2
        win = holdem_eval(board, [hand1, hand2])
        if win[2] == hand1:
            one += 1
        else:
            two += 1
        #if (one + two) % 100000 == 0: print "%d iterations" % (one + two)
    print "One wins %f%% of the time" % ((float(one)/(one+two)) * 100)
