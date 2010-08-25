from csv import reader
from math import floor
from datetime import date
from random import Random
rand = Random().random

from nodebox.graphics import Context, HSB
from AppKit import NSApplication
NSApplication.sharedApplication().activateIgnoringOtherApps_(0)

#here's what I want: 800 dates evenly distributed between today and 1970
def datesamples():
    today = date.today()
 
def getastock(r):
    prices = []
    _, sym, dt, _, _, _, _, _, adj_close = r.next()
    prices.append((dt, float(adj_close)))

    while 1:
        _, nextsym, dt, _, _, _, _, _, adj_close = r.next()
        if nextsym != sym:
            break
        prices.append((dt, float(adj_close)))

    #now prices contains 2-tuples of a given stock
    #for the first try, let's just sample 800 values
    prices = [p[1] for p in prices]
    return prices

def sample(lst, n):
    """from list @lst, sample @n values"""
    assert type(n) == type(0)

    if n >= len(lst):
        #XXX: or throw an exception?
        return lst

    samples = []
    nth = len(lst) / float(n)
    indices = [0] + [int(round(nth*i)) for i in range(1,n)]

    if len(indices) != n:
        print "fuck.", len(indices), n, len(lst)
        raise Exception("wrong number of samples")
    if indices[-1] >= len(lst):
        print "fuck.", len(indices), n, indices[-1], len(lst)
        raise Exception("invalid indices")
    
    return [lst[i] for i in indices]

w = 800.
h = 800.

def getcontext():
    ctx = Context()
    ctx.size(w, h)

    #ctx.stroke(0,0,0,1)
    ctx.strokewidth(1)
    ctx.colormode(HSB)

    return ctx

def graph(samples, ctx):
    zero = samples[0]
    maxs = max(samples)
    mins = min(samples)
    maxdiff = max(maxs-zero, zero-mins)
    pct = (h/2) / maxdiff
    samples = [(h/2) + ((s-zero) * pct) for s in samples]

    if max(samples) > 800.01 or min(samples) < -0.01:
        print max(samples), min(samples), zero, maxs, mins, maxdiff
        raise Exception("thppppt")

    ctx.stroke(ctx.color(rand(), .8, rand(), 1))

    l = len(samples)
    for x, sample in enumerate(samples):
        #if x == l-1: next = sample
        #else:        next = samples[x+1]
        ctx.line(x, sample, x+1, sample)

letter="A"
def get_reader():
    global letter
    r = reader(open("NYSE/NYSE_daily_prices_%s.csv" % letter))
    #skip header
    _ = r.next()
    letter = chr(ord(letter)+1)
    return r

r = get_reader()
ctx = getcontext()
for i in range(100):
    try:
        prices = getastock(r)
    except StopIteration:
        r = get_reader()
        prices = getastock(r)
    samples = sample(prices, 800)
    graph(samples, ctx)
    del prices, samples

ctx.save("stocks.png")
