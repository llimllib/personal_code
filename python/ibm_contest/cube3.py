#!/usr/bin/python2.4
from combine import xpermutations

pool = range(36)
SIX = tuple(range(6))
SIXES = tuple(range(0, 36, 6))

#XXX: remember to check for flips
def xcols(cube):
    for row in SIXES:
        cols = [0 for i in range(6)]
        for exp in SIX:
            for i in SIX:
                if cube[row+i] & 2**exp:
                    cols[exp] += 2**i
        yield cols

def xstacks(cube):
    for stack in SIX:
        stacks = [0 for i in range(6)]
        for exp in SIX:
            for i in SIX:
                if cube[(i*6)+stack] & 2**exp:
                    stacks[exp] += 2**i
        yield stacks

def is_magic(cube, gen):
    elts = []
    for row in gen(cube):
        for elt in row:
            if elt in elts:
                return False
            else:
                elts.append(elt)
    return True

def bin(n):
    #not fast
    s = []
    if not n: return ''.rjust(6, '0')
    while n:
        if n&1: s.insert(0, '1')
        else: s.insert(0, '0')
        n >>= 1
    return ''.join(s).rjust(6, '0')

def print_cube(cube, fout):
    binary = [bin(c) for c in cube]
    for elt in binary:
        for i in SIX:
            print >>fout, elt,
        print >>fout

def gen_cubes():
    fout = ('cubes.dat', 'w')
    count = 0
    found = 0
    for cube in xpermutations(pool):
        count += 1
        if count % 1000000 == 0: print count, found
        if is_magic(cube, xcols):
            if is_magic(cube, xstacks):
                found += 1
                print_cube(cube)

if __name__ == '__main__':
    gen_cubes()
