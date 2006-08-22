#!/usr/bin/python
import random, algos

def gen_ref_str(P, p, e, m, t, l, f, rand):
    """generate a reference string, as defined on p. 499

    P: length of virtual memory
    p: current locality
    e: extent of the current locality
    m: rate of motion
    t: likelihood of p's transition to a new locality
    l: number of pages to generate
    f: file to print them to
    rand: random number generator with ops randint() and rand()
    """
    for i in range(l):
        for j in range(m):
            print >>f, rand.randint(p, p+e) % P,
        r = rand.random()
        if r < t: p = rand.randint(0, P-1)
        else: p = (p+1) % P

def test_algo(a, f, mem, tau=0):
    """run the page replacement object (a) and collect statistics.

    The page replacement algorithm can do any initialization in __init__, and
    should accept a pointer to the main memory in this function. The class
    should also have a function replace() which takes one argument, the page
    which needs to be swapped in. It should return an index into the mem array
    of the cell to be swapped out.
    
    a: a reference to the page replacement object, described above
    f: the file containing the RS
    mem: a reference to the main memory
    """
    ns = f.read().split()   #read the ref string from the file
    if tau: a = a(mem, ns, tau) #initialize an instance of a local algorithm
    else:   a = a(mem, ns)      #initialize an instance of a global algorithm
    pf = 0                  #number of page faults
    while 1:
        try:
            n = ns.pop(0)        #get a new n and remove it from the front
        except IndexError: break #this means we're through ns
        if n not in mem:
            pf += 1         #there's a page fault
            i = a.replace(n)#so replace n
            mem[i] = n      #and put it where the algorithm suggests
        else: a.reference(n)
    if tau: 
        av_ws = float(a.cur_frames) / a.n_accesses
        return (pf, av_ws)
    else: return pf
    
if __name__=="__main__":
    """Create an address space of 1000 cells. The actual memory size will be
    100 cells"""
    rand = random.Random()
    #uncomment to regenerate the reference string; currently generates 10000
    #addresses
    #ref_str = file('ref_str.out', 'w')
    #gen_ref_str(1000, 0, 20, 10, .10, 1000, ref_str, rand)
    #for s in dir(algos):              #test all algorithms
    #    ref_str = file('ref_str.out') #reinitialize file
    #    if s.find('_local_') > -1:
    #        tau = 20
    #        work_set = [-1 for i in range(tau)]
    #        pf, ws_size = test_algo(algos.__getattribute__(s), ref_str, \
    #                                work_set, tau)
    #        print "%s had %d page faults" % (s, pf)
    #        print "%s averaged %f pages in memory" % (s, ws_size)
    #    elif s[-8:] == '_replace':
    #        main_mem = [-1 for i in range(100)]     #and memory
    #        pf = test_algo(algos.__getattribute__(s), ref_str, main_mem)
    #        print "%s had %d page faults" % (s, pf)

    #debugging code

    ref_str = file('ref_str.out')           #reinitilize file
    #note how we're testing local funcs
    #also, we haven't yet gotten avg working set size data
    tau = 20
    main_mem = [-1 for i in range(tau)]     #and memory
    pf, avg = test_algo(algos.vmin_local_replace, ref_str, main_mem, tau)
    print "vmin_replace had %d pagefaults and an avg of %f" % (pf, avg)
