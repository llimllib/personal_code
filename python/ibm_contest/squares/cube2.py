
SIX = range(6)
THIRTYSIX = range(36)

def flip(n, b):
    """
    flip a number's binary representation. i.e. 6 -> 3 (110 -> 011) and
    8 -> 1 (1000 -> 0001) and pad to 'b' bytes
    """
    #can be made much faster if the number of bits in i is known (skip integer
    #conversion) but it's pretty unnecessary if we pre-generate our flips
    s = []
    if not n: return 0
    while n:
            if n&1: s.append('1')
            else: s.append('0')
            n >>= 1
    l = len(s)
    s.extend(['0' for i in range(b-l)])
    return int(''.join(s), 2)

def bin(n):
    #not fast
    s = []
    if not n: return 0
    while n:
        if n&1: s.insert(0, '1')
        else: s.insert(0, '0')
        n >>= 1
    return ''.join(s)

def xrow(n):
    for i in SIX:
        row = 0
        #get bit at 2**[0,6,12,18,24,30] then 2**[1,7,13,19,25,31], etc
        for exp in xrange(i, i+31, 6):
            if n & (2**exp):
                row += 2**(exp / 6)
        yield row

#NOTE: precalc xranges?
def xcol(n):
    for i in SIX:
        col = 0
        #get bit at 2**[0,1,2,3,4,5] then 2**[6,7,8,9,10,11], etc
        for exp in xrange(6*i, (6*i)+6):
            if n & (2**exp):
                col += 2**(exp % 6)
        yield col

def xstack(cube):
    for exp in THIRTYSIX:
        stack = 0
        for i, square in enumerate(cube):
            if square & 2**exp:
                stack += 2**i
        yield stack

flips = {}
for i in xrange(2**6):
    flips[i] = flip(i, 6)

def gen_squares(start, lim, f):
    sq = start
    squares = f
    if sq >= lim: raise "start !< lim"
    while sq < lim:
        broken = False
        rows = []
        cols = []
        for row in xrow(sq):
            if row not in rows and flips[row] not in rows:
                rows.append(row)
            else: 
                broken = True
                break
        if not broken:
            for col in xcol(sq):
                if col not in cols and flips[col] not in cols:
                    cols.append(col)
                else: 
                    broken = True
                    break
        if not broken: 
            print >>squares, sq
        sq += 1

gen_squares(1000000, 2**37, file('squares.dat', 'w'))
