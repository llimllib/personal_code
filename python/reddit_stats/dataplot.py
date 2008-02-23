import cPickle
from pylab import plot, show
from numpy import median, average, array, std

def group(x, n):
    i = 0
    while i < len(x):
        yield x[i:i+n]
        i += n

if __name__=="__main__":
    stories = cPickle.load(file("stories.pkl"))

    n_comments = array([x[1] for x in stories])


    comments = list(group(n_comments, 250))

    import pylab
    l1, l2, l3 = pylab.plot(map(average, comments), 'b-', 
                            map(median, comments),  'g-',
                            map(std, comments),     'r-')
    pylab.setp(l1, label="average")
    pylab.setp(l2, label="median")
    pylab.setp(l3, label="standard deviation")
    pylab.legend()

    #ipython doesn't require this for some reason? reg python does.
    pylab.show()
