#!/usr/bin/env python
import lsi, cPickle, sets, sys
import numarray as na

def load_artists():
    f = file('artist_data-20040925.txt')
    d = {}
    for line in f:
        if line:
            line = line.split('\t')
            try: d[line[0]] = line[1].strip()
            except IndexError: pass
    return d

def get_artist_index(name, d, l):
    for key in d:
        if d[key] == name: idx = key
    return l.index(idx)

def get_artist_name(idx, d, l):
    return d[l[idx]]

artist_dict = load_artists()
artist_list = artist_dict.keys()
shadow = get_artist_index('DJ Shadow', artist_dict, artist_list)

fin = file('lsi.out', 'rb')
up = cPickle.Unpickler(fin)
lsi = up.load()
fin.close()

shadow = [(lsi[shadow,i], i) for i in range(len(lsi))]
shadow.sort
for i in range(10):
    print '%s %f' % (get_artist_name(shadow[i][1]), shadow[i][0])
