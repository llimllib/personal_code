from glineenc import encode_pairs
from cPickle import dump

h = [x.strip().split("\t") for x in file("csa_latlon.csv").readlines()]
hoods = {}
for _, lon, lat, __, ___, hood, ____, _____ in h[1:]:
    hoods.setdefault(hood, []).append((float(lat), float(lon)))

borders = {}
for hood, pairs in hoods.iteritems():
    borders[hood] = encode_pairs(pairs)

dump(borders, file("bnia_hoods.pkl", "w"))
