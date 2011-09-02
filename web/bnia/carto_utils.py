from decodegmap import decode_points

#return true if p lies below the line v1->v2, false otherwise
#v1, v2, and p should be tuples of floats. FLOATS not INTS duders
#note that these points are (lat, lon), which means (y, x)
def line_is_above(v1, v2, p):
    #eliminate the divide by zero case; if there's no slope
    #then x1 == x2 and we return whether the point is on that x
    if v2[1] == v1[1]: return p[1] == v1[1]

    m = (v2[0]-v1[0]) / (v2[1]-v1[1])
    b = v1[0] - (m*v1[1])
    return (m*p[1]) + b > p[0]

#the decoding function returns a list [lat, lon, lat1, lon1, ...]
#so turn it into [(lat, lon), (lat1, lon1), ...]
def make_pairs(encoded_border):
    pairs = []
    pts = decode_points(encoded_border)
    it = iter(pts)
    for i in it:
        pairs.append((i, it.next()))
    return pairs

#return true if p1 <= px < p2 or p2 <= px < p1
#needs to be an open interval to avoid the problem of a point being
#on the same plane as an intersection of two lines
def btw(p1, p2, px):
    return min(p1, p2) <= px < max(p1, p2)

def debug(p1, p2, la, lo):
    x1, y1 = p1
    x2, y2 = p2
    return "%.5f, %.5f | %.5f, %.5f | %.5f, %.5f" % (x1, y1, x2, y2, la, lo)

def is_in(pt, border):
    lat, lon = pt
    borders = make_pairs(border[0])
    intersections = 0
    bl = len(borders)
    for i, p1 in enumerate(borders):
        #loop around to test the last point against the first
        p2 = borders[(i+1) % bl]
        if btw(p1[1], p2[1], lon) and line_is_above(p1, p2, pt):
            intersections += 1

    #if the number of intersections is odd, we're in the poly
    #print "found pt in %s intersections" % intersections
    if intersections % 2 == 1:
        return True
    return False

def centroidpt(border):
    pts = decode_points(border[0])

    lats = pts[::2]
    lons = pts[1::2]
    assert len(lats) == len(lons)
    
    return (sum(lats)/float(len(lats)), sum(lons)/float(len(lons)))

def bounding_box(border):
    borders = decode_points(border[0])

    lats = borders[::2]
    lons = borders[1::2]
    assert len(lats) == len(lons)

    def minmax(lst):
        min = max = lst[0]
        for l in lst[1:]:
            if l < min: min = l
            elif l > max: max = l
        return min, max

    minlat, maxlat = minmax(lats)
    minlon, maxlon = minmax(lons)
    return ((minlat, minlon), (maxlat, maxlon))

def centerpt(border):
    (minlat, minlon), (maxlat, maxlon) = bounding_box(border)
    return ((minlat + maxlat) / 2, (minlon + maxlon) / 2)
