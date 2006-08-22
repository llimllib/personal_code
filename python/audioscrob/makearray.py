import numarray, sets, lsi, sys, cPickle

def loadfile(fname):
    f = file(fname, 'r')
    x = f.read().split('\n')
    x = [line.split('\t') for line in x]
    return x[:-1]

def makesets(data):
    users = sets.Set()
    artists = sets.Set()
    for line in data:
        users.add(line[0])
        artists.add(line[1])
    return (list(users), list(artists))

def fillarr(data, outarr, users, artists):
    for line in data:
        outarr[artists.index(line[1]),users.index(line[0])] = int(line[2])

def write_mm(data, fout, users, artists):
    print >>fout, \
"""%%%%MatrixMarket matrix coordinate integer general
%%Matrix containing user-artist pairs. x is the user array index, y is
%%the artist array index. artists and users are stored in artist_user.pickle
%d %d %d""" % (len(artists), len(users), len(data))
    for line in data:
        #NOTE THE +1s!!! They are required for MatrixMarket format (no 0,0) 
        print >>fout, artists.index(line[1])+1, users.index(line[0])+1, line[2]
        
def write_st(data, fout, users, artists):
    """output a matrix in svdlibc's native sparse text format
    
    columns are users, rows are artists. This format sucks donkey balls.
    """
    print >>fout, len(artists), len(users), len(data)
    count = 0
    for user in users:
        count += 1
        points = [x for x in data if x[0]==user]
        #def cmpfunc(a, b):
        #    if artists.index(a[1]) > artists.index(b[1]): return 1
        #    return -1
        #points.sort(cmpfunc)
        print >>fout, len(points)
        for p in points:
            data.remove(p)
            print >>fout, artists.index(p[1]), '%s' % p[2]
    print >>fout
    print "output column count: %d" % (count)

data = loadfile('big.txt')
users, artists = makesets(data)
write_st(data, file('big.st', 'w'), users, artists)
#write_mm(data, file('big.mtx', 'w'), users, artists)
#a = numarray.zeros((len(artists), len(users)), type="Int")
#fillarr(data, a, users, artists)
#fout = file('array.out', 'wb')
#cp = cPickle.Pickler(fout, -1)
#cp.dump(a)
#fout.close()
#
#fout = file('artist_user.pickle', 'wb')
#cp = cPickle.Pickler(fout, -1)
#cp.dump((artists, users))
#fout.close()
