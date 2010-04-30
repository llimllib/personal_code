import time
from clp_perm import NPermutation
from pprint import pprint as pp

def perm1(lst):
    """implementation of algo L, page 2, fascicle 2b, TAOCP
    
    Note that he uses a 1-based array, so you should subtract 1 from all his
    indices.

    Also note that this algorithm modifies lst in place.
    """
    yield lst 

    if len(lst) == 1: return

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

def perm1b(lst):
    """A modification of perm1 to support cycling. Permutes until the list has reached
    a cycle instead of until it's reverse sorted
    """
    initial = lst[:]
    yield lst 

    if len(lst) == 1: return

    n = len(lst) - 1
    while 1:
        j = n - 1
        while lst[j] >= lst[j+1]:
            j -= 1
            #because this algorithm iterates in lexicographic order,
            #we know that lst is reverse sorted right now
            #if j == -1: return #terminate
            if j == -1:
                lst.reverse()
                if lst == initial: #if lst was sorted to begin with
                    return
                yield lst
                j = n-1
                break
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
        if lst == initial:
            return
        yield lst

def perm1c(lst):
    """A modification of perm1b to return a copied list instead of modified in place"""
    initial = lst[:]
    yield lst 
    lst = lst[:]

    if len(lst) == 1: return

    n = len(lst) - 1
    while 1:
        j = n - 1
        while lst[j] >= lst[j+1]:
            j -= 1
            #because this algorithm iterates in lexicographic order,
            #we know that lst is reverse sorted right now
            #if j == -1: return #terminate
            if j == -1:
                lst.reverse()
                if lst == initial: #if lst was sorted to begin with
                    return
                yield lst
                lst = lst[:]
                j = n-1
                break
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
        if lst == initial:
            return
        yield lst
        lst = lst[:]

def perm2(lst):
    """implementation of variation of algo L, exercise 1, fascicle 2b, TAOCP
    
    Note that he uses a 1-based array, so you should subtract 1 from all his
    indices.

    Also note that this algorithm modifies lst in place.
    """
    yield lst
    if len(lst) == 1: return
    if len(lst) == 2:
        lst[0], lst[1] = lst[1], lst[0]
        yield lst
        return

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

    if len(lst) == 1: return
    if len(lst) == 2:
        lst[0], lst[1] = lst[1], lst[0]
        yield lst
        return

    n = len(lst) - 1
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
            if j < 0: return
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
    if len(lst) == 1: return
 
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
    for perm in p: yield perm

def clp_perm(l):
    """yanked and modified from: 
    http://mail.python.org/pipermail/python-list/2002-November/170393.html"""
    if len(l) == 1: yield l; return

    pop, insert, append = l.pop, l.insert, l.append

    def halfperm():
        ll = l
        llen = len(ll)
        if llen == 2:
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

def pyorg_perm(iterable):
    """The pseudocode given for Python's C implementation of permutation
    from: http://svn.python.org/view/python/branches/py3k/Modules/itertoolsmodule.c?view=markup
    but it only works in python 2k :)"""
    pool = tuple(iterable)
    n = len(pool)
    r = n
    indices = range(n)
    cycles = range(n-r+1, n+1)[::-1]
    yield list(pool[i] for i in indices[:r])
    while n:
        for i in reversed(range(r)):
            cycles[i] -= 1
            if cycles[i] == 0:
                indices[i:] = indices[i+1:] + indices[i:i+1]
                cycles[i] = n - i
            else:
                j = cycles[i]
                indices[i], indices[-j] = indices[-j], indices[i]
                yield list(pool[i] for i in indices[:r])
                break
        else:
            return

def xcombinations(items, n):
    if n==0: yield []
    else:
        for i in xrange(len(items)):
            for cc in xcombinations(items[:i]+items[i+1:],n-1):
                yield [items[i]]+cc

def xpermutations(items):
    return xcombinations(items, len(items))

def test(f):
    #note! all algorithms presented here expect the array to be sorted and not
    #empty.
    tests = [([1], [[1]]),
                ([1,2], [[1,2], [2,1]]),
                ([1,2,3], [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], 
                            [3,2,1]]),
                ([1,2,3,4], [[1,2,3,4],[1,2,4,3],[1,3,2,4],[1,3,4,2],[1,4,2,3],
                            [1,4,3,2],[2,1,3,4],[2,1,4,3],[2,3,1,4],[2,3,4,1],
                            [2,4,1,3],[2,4,3,1],[3,1,2,4],[3,1,4,2],[3,2,1,4],
                            [3,2,4,1],[3,4,1,2],[3,4,2,1],[4,1,2,3],[4,1,3,2],
                            [4,2,1,3],[4,2,3,1],[4,3,1,2],[4,3,2,1]]),
                (['a','b','c'], [['a','b','c'],['a','c','b'],['b','a','c'],
                                ['b','c','a'],['c','a','b'],['c','b','a']]),
            ]
    failure = False
    for example, expected in tests:
        actual = []
        try:
            for p in f(example):
                #copy array, since modifications occur in-place
                actual.append(p[:])
            if [expected.count(p) for p in actual] != [1] * len(expected) or \
                [actual.count(p) for p in expected] != [1] * len(expected):
                print "%s failed on %s (%s)" % (f.__name__, example, actual)
                pp(expected)
                pp(actual)
                failure = True
        except Exception, msg:
            print "Error in %s on input %s" % (f.__name__, example)
            pp(expected)
            pp(actual)
            failure = True

    if not failure:
        print "%s passed" % f.__name__
    return not failure

def speed(f):
    #and now we can test for speed
    arr = range(10)
    facten = 10*9*8*7*6*5*4*3*2
    count = 0
    t1 = time.time()
    for i in f(arr): #list(f(arr)) stores it in memory, let's avoid that
        count += 1    
    t2 = time.time()
    if count != facten:
        raise "Incorrect number of iterations for %s!" % f.__name__
    print '%s: %f' % (f.__name__, t2 - t1)

try: all
except:
    def all(iterable):
        for i in iterable:
            if not i: return False
        return True

if __name__ == '__main__':
    #TODO: remember how to compile this shit :) wasn't that fast anyway
    #from permute_c import Permute2
    funcs = [perm1, perm1b, perm1c]
    #funcs = [perm1, perm2, perm3, perm4, clp_perm, pyorg_perm]
    #funcs = [xpermutations, perm5] #the graveyard of the too-slow
    try:
        import probstat
        funcs.append(probstat.Permutation)
    except: pass

    tests = [test(f) for f in funcs]
    if all(tests):
        for f in funcs: speed(f)
