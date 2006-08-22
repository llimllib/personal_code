try: 
    import Gnuplot
    G = Gnuplot.Gnuplot(debug=0)
except: G = None

def wait_for_input():
    raw_input("press enter to exit graph")

def plot_bananas(pts1, pts2, filename, m=None, b=0, title='', \
    x_label='', y_label='', x_range=(1,100), y_range=(1,100), output=None):
    """plot a line and a set of points
    
    TODO: extend to arbitrary amt of sets of points **pts"""
    if(G):
        G.reset()
        p1 = Gnuplot.Data(pts1, title="apples", with="points 1 2")
        p2 = Gnuplot.Data(pts2, title="bananas", with="points 3")
        if m: line = Gnuplot.Func ('%f + (%f) * x' % (b, m))
        G.title("apples and bananas")
        G.xlabel("size")
        G.ylabel("yellowness")
        G.set_range('xrange', (0,100))
        G.set_range('yrange', (0,100))
        if m: G.plot(line, p1, p2)
        else: G.plot(p1, p2)
        G.hardcopy(filename, terminal='png')
        wait_for_input()

def plot_line(m, b, xrange=None, yrange=None):
    """plot a line with slope m and bias b, currently using Gnuplot"""
    if(G):
        line = Gnuplot.Func ('%f + (%f) * x' % (b, m))
        if xrange: G.set_range('xrange', xrange)
        if yrange: G.set_range('yrange', yrange)
        G.plot(line)
        wait_for_input()
