import timeit, commands, random, os

def wc(f):
    l = ' '
    c = 0
    for line in f:
        c += 1
    return c

def rline1(f):
    wct = wc(f)
    f.seek(0)
    r = random.randint(0, wct)
    c = 0
    while c < r:
        for line in f: c += 1
    return f.next()
            
def rline2(f):
    bytes = os.stat(f.name)[6]
    r = random.randint(0, bytes)
    f.seek(r)
    f.

if __name__ == '__main__':
    f = file('osfortune')
    for func in ('wc1', 'wc2', 'wc3'):
        t = timeit.Timer('%s(f)' % func, 'from __main__ import %s, f' % func)
        print '%s: %f' % (func, t.timeit(1000))
