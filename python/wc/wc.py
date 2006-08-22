import timeit, commands

def wc1(f):
    l = ' '
    c = 0
    f.seek(0)
    while l:
        l = f.readline()
        c += 1
    return c

def wc2(f):
    l = ' '
    c = 0
    f.seek(0)
    for line in f:
        c += 1
    return c

def wc3(f):
    l = ' '
    c = 0
    f.seek(0)
    while l:
        l = f.read(1024)
        c += l.count('\n')
    return c

if __name__ == '__main__':
    f = file('osfortune')
    for func in ('wc1', 'wc2', 'wc3'):
        t = timeit.Timer('%s(f)' % func, 'from __main__ import %s, f' % func)
        print '%s: %f' % (func, t.timeit(1000))
