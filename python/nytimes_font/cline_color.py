from math import sqrt, ceil, floor

#next we need lines whose color changes somewhat randomly
def cline(x1,y1,x2,y2,clr):
    w=4
    nostroke()
    fill(clr)
    oval(x1-w/2, y1-w/2, w, w)
    stroke(clr)
    strokewidth(w)
    line(x1,y1,x2,y2)
    nostroke()
    oval(x2-w/2, y2-w/2, w, w)

def ccline(x1,y1,x2,y2):
    minl, maxl = 10, 50
    dist = sqrt((x1**2 + x2**2) + (y1**2 + y2**2))
    maxsplit, minsplit = floor(dist/minl), ceil(dist/maxl)
    splits = random(minsplit, maxsplit)
    colors = [color(1,0,0), color(1,0,0), color(1,0,1)]
    lens = [random(minl, maxl) for s in range(splits)]
    print "wtf", dist, minsplit, maxsplit
    print lens, sum(lens), dist
    while sum(lens) > dist:
        lens = [random(minl, maxl) for s in range(splits)]
        print lens, sum(lens), dist
        break
    c = colors[0]
    cur = (x1,y1)
    m = (y2-y1) / (x2-x1) if x2 != x1 else "vertical"
    xdir = 1 if x2 > x1 else -1
    ydir = 1 if y2 > y1 else -1
    for l in lens:
        if m == "vertical":
            dx, dy = 1, 0
        else:
            dy = (1 / (sqrt(1/m**2) + 1)) * ydir
            dx = (l-dy) * xdir
        cur = (x1+dx, y1+dy)
        cline(x1,y1,cur[0],cur[1],c)
        c = colors[random(0, len(colors)-1)]

ccline(10,10,10,30)
ccline(15,10,30,30)
ccline(50,10,35,30)
ccline(10,35,15,50)
ccline(25,35,20,50)
ccline(25,50,40,45)
ccline(30,35,45,40)
