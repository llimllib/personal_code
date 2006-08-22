import time
from clp_perm import NPermutation

def perm1(lst):
    """implementation of algo L, page 2, fascicle 2b, TAOCP
    
    Note that he uses a 1-based array, so you should subtract 1 from all his
    indices.

    Also note that this algorithm modifies lst in place.
    """
    yield lst #should I assume you've processed the first permutation?
              #for now, I'll work with algorithms which return all permutations
              #including the trivial one (makes usage prettier)
    n = len(lst) - 1
    while 1:
        j = n - 1
        while lst[j] >= lst[j+1]:
            j -= 1
            if j == -1: return #terminate
        l = n
        while lst[j] >= lst[l]:
            l -= 1
        lst[j], lst[l] = lst[l], lst[j]
        k = j + 1
        l = n
        while k < l:
            lst[k], lst[l] = lst[l], lst[k]
            k += 1
            l -= 1
        yield lst

def perm2(lst):
    """implementation of variation of algo L, exercise 1, fascicle 2b, TAOCP
    
    Note that he uses a 1-based array, so you should subtract 1 from all his
    indices.

    Also note that this algorithm modifies lst in place.
    """
    yield lst
    n = len(lst) - 1
    while 1:
        #half the time, j = n-1, so we can just switch n-1 with n
        if lst[-2] < lst[-1]:
            lst[-2], lst[-1] = lst[-1], lst[-2]
        else:
            #and now we know that n-1 > n, so start j at n-2
            j = n - 2
            while lst[j] >= lst[j+1]:
                j -= 1
                if j == -1: return #terminate
            l = n
            while lst[j] >= lst[l]:
                l -= 1
            lst[j], lst[l] = lst[l], lst[j]
            k = j + 1
            l = n
            while k < l:
                lst[k], lst[l] = lst[l], lst[k]
                k += 1
                l -= 1
        yield lst

def perm3(lst):
    """implementation of Knuth's answer to exercise 1, fascicle 2b, TAOCP"""
    yield lst
    n = len(lst) - 1
    #need to special case a 2-element list
    if n == 1:
        yield [lst[1], lst[0]]
    while 1:
        #half the time, j = n-1, so we can just switch n-1 with n
        if lst[-2] < lst[-1]:
            lst[-2], lst[-1] = lst[-1], lst[-2]
        #let's special case the j = n-2 scenario too!
        elif lst[-3] < lst[-2]:
            if lst[-3] < lst[-1]:
                lst[-3], lst[-2], lst[-1] = lst[-1], lst[-3], lst[-2]
            else:
                lst[-3], lst[-2], lst[-1] = lst[-2], lst[-1], lst[-3]
        else:
            #and now we know that n-2 > n-1, so start j at n-3
            j = n - 3
            if j < 0: return #need to terminate here because an index of -1
                             #is perfectly valid in python (hit when n = 2)
            y = lst[j]
            x = lst[-3]
            z = lst[-1]
            while y >= x:
                j -= 1
                if j < 0: return #terminate
                x = y
                y = lst[j]
            if y < z:
                lst[j] = z
                lst[j+1] = y
                lst[n] = x
            else:
                l = n - 1
                while y >= lst[l]:
                    l -= 1
                lst[j], lst[l] = lst[l], y
                lst[n], lst[j+1] = lst[j+1], lst[n]
            k = j+2
            l = n-1
            while k < l:
                lst[k], lst[l] = lst[l], lst[k]
                k += 1
                l -= 1
        yield lst

def perm4(lst):
    """Single transposition permutation algorithm. lst must not have repeated
        elements in it.
    """
    if max([lst.count(x) for x in lst]) > 1: raise "no repeated elements"
    yield lst
    n = len(lst) - 1
    c = [0 for i in range(n+1)]
    o = [1 for i in range(n+1)]
    j = n
    s = 0
    while 1:
        q = c[j] + o[j]
        if q >= 0 and q != j+1:
            lst[j-c[j]+s], lst[j-q+s] = lst[j-q+s], lst[j-c[j]+s]
            yield lst
            c[j] = q
            j = n
            s = 0
            continue
        elif q == j+1:
            if j == 1: return
            s += 1
        o[j] = -o[j]
        j -= 1

def perm5(lst):
    p = NPermutation(len(lst))
    for perm in p: yield p

def clp_perm(l):
    pop, insert, append = l.pop, l.insert, l.append

    def halfperm():
        ll = l
        llen = len(ll)
        if llen <= 2:
            yield ll
            return
        aRange = range(llen)
        v = pop()
        for p in halfperm():
            for j in aRange:
                insert(j, v)
                yield ll
                del ll[j]
        append(v)

    for h in halfperm():
        yield h
        h.reverse()
        yield h
        h.reverse()


def test(f):
    #first let's do a simple test for correctness
    arr = [1,2,3]
    perms = [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]
    perms_ = []
    for i in f(arr):
        print i
        perms_.append(i[:])
    if [perms.count(p) for p in perms] != [1,1,1,1,1,1]:
        print perms_
        raise "%s is not correct!" % f.__name__
    
    #and now we can test for speed
    arr = [1,2,3,4,5,6,7,8,9,10]
    facten = 10*9*8*7*6*5*4*3*2
    count = 0
    t1 = time.time()
    for i in f(arr): #list(f(arr)) stores it in memory, I'd like to avoid that
        count += 1    
    t2 = time.time()
    if count != facten:
        raise "Incorrect number of iterations for %s!" % f.__name__
    print '%s: %f' % (f.__name__, t2 - t1)

if __name__ == '__main__':
    #test(perm1)
    #test(perm2)
    #test(perm3)
    #test(perm4)
    #test(clp_perm)
    test(perm5)
    try:
        import probstat
        test(probstat.Permutation)
    except ImportError:
        print "probstat not installed"
