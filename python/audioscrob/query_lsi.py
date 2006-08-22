#!/usr/bin/env python
import lsi, cPickle, sets, sys
import numarray as na

def load_svd(sfile, ufile, vfile):
    """loads dense svd files as output by svdlibc"""
    n_s = int(sfile.readline())
    S = na.zeros((n_s, n_s), type="Float32") #store S as a column vector
    for i in range(n_s):
        S[i,i] = float(sfile.readline())
    assert sfile.readline() == ''
    
    rows, columns = [int(a) for a in ufile.readline().split()]
    U = na.zeros((rows, columns), type="Float32")
    row = 0
    for line in ufile:
        col = 0
        for n in line.split():
            U[row, col] = float(n)
            col += 1
        row += 1

    rows, columns = [int(a) for a in vfile.readline().split()]
    V = na.zeros((rows, columns), type="Float32")
    row = 0
    for line in vfile:
        col = 0
        for n in line.split():
            V[row, col] = float(n)
            col += 1
        row += 1

    return U, S, V

up = cPickle.Unpickler(file('artist_user.pickle', 'rb'))
artists, users = up.load()

#U is artists
U, S, V = load_svd(file('big_s', 'r'), file('big_ut', 'r'), file('big_vt', 'r'))
#I believe that U is already transposed
tt = na.dot(na.transpose(U), na.dot(na.dot(S, S), U))
fout = file('lsi.out', 'wb')
cp = cPickle.Pickler(fout, -1)
cp.dump(tt)
fout.close()

