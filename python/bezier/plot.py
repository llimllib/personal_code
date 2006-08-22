import Gnuplot

#original points (part 1)
#points = [(4,1), (28,48), (50,42), (40,5)]
#these points give a loop (part 3)
#points = [(4,1), (60,0), (50,42), (40,5)]
#let's make a 'c' (part 4)
points = [(10,10), (0,15), (0,35), (10,40)]

lines = []

fname = "points"
f = file(fname, 'w')
for i in range(len(points)):
    print >>f, points[i][0], points[i][1]
    #lines from i to i+1
    if i + 1 < len(points):
        lines.append("%d + (%d - %d) * t" %\
                     (points[i][0], points[i+1][0], points[i][0]))
        lines.append("%d + (%d - %d) * t" %\
                      (points[i][1], points[i+1][1], points[i][1]))
f.flush()

bezier = "%d*(1-t)**3 + 3*%d*t*(1-t)**2 + 3*%d*t**2*(1-t) + %d*t**3"
x = bezier % (points[0][0], points[1][0], points[2][0], points[3][0])
y = bezier % (points[0][1], points[1][1], points[2][1], points[3][1])

g = Gnuplot.Gnuplot(debug=1)
g('set parametric')
g('set samples 100')
g('set trange[0:1]')

#part 1 range
#g('set xrange[-60:100]')
#g('set yrange[-100:60]')
#part 3 range
g('set xrange[-10:100]')
g('set yrange[-30:80]')
g('set nokey')

#put linebreaks between the plots
plot = "plot %s,\\\n%s, \"%s\"" % (x,y,fname)
for l in lines: plot += ",\\\n%s" % l
g(plot)
raw_input("press enter")

#cool but not really useful
def get_bezier(a, b, c, d):
    return lambda t: a*(1-t)**3+3*b*t*(1-t)**2+3*c*t**2*(1-t)+d*t**3
