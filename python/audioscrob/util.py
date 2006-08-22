import numarray, sets, sys, cPickle, Itpl, lsi

def load_artists():
    f = file('artist_data-20040925.txt')
    d = {}
    for line in f:
        if line:
            line = line.split('\t')
            try: d[line[0]] = line[1].strip()
            except IndexError: pass
    return d

def pairwho(pair, artist_info):
    one = artist_info[artists[pair[0]]]
    two = artist_info[artists[pair[1]]]
    return (one, two)

def printpairs(pairs, artist_info, fout=sys.stdin):
    for p in pairs:
        print >>fout, pairwho(p, artist_info)

artist_info = load_artists()
#up = cPickle.Unpickler(file('artist_user.pickle', 'rb'))
#artists, users = up.load()
up = cPickle.Unpickler(file('lsi.pickle', 'rb'))
data = up.load()

fout = file('shadow.co', 'w')
co = lsi.calc_co_occur(lsi.make_binary_matrix(data))
for x in co[3116]:
    print >> fout, x

#'DJ Shadow' == artist 118, index 3116 in artists
#output DJ Shadow info
#fout = Itpl.ItplFile(file('shadow.out', 'w'))
#count = 0
#for val in data[3116]:
#    fout.write("$count\t$val\n")
#    count += 1
#find DJ Shadow artist #, index
#for key in artist_info:
#    if artist_info[key] == 'DJ Shadow':
#        print key
#        print artists.index(key)
#maxes = find_max(res)
#printpairs(maxes, artist_info, file('out.dat', 'w'))
