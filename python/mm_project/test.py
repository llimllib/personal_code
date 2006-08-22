#!/usr/bin/python
from mem import *
from random import gauss, randint

def test_mem(m, iter, a, d, pcb, next_fit, fout):
    """test a memory manager

    m is a reference to the memory manager
    iter is the number of iterations to use in testing it
    a is the mean of the gaussian distribution of process sizes
    d is the standard deviation of the gaussian distribution
    next-fit is true if testing the next-fit algorithm, false if testing the
             first-fit algorithm
    fout is the file to print out graphing data to"""
    pcb = {}
    m.__init__(pcb)
    avg_util = 0.0
    avg_n = 0.0
    n_count = 0
    request_q = []
    for i in range(iter):
        while 1:
            req_from_q = 0
            try: n = request_q.pop()
            except IndexError:
                n = (int(gauss(a, d)) % MEM_SIZE) + 1
                avg_n += n
                n_count += 1
            #print "requesting process of size %d" % n
            try: start = m.mm_request(n, next_fit)
            except BlockTooLarge:
                #print "request too large"
                break
                request_q.append(n)
            #print "process given space at %d" % start
            #m.print_memory()
            #print id(pcb)
        avg_util += m.get_util()
        try: p = pcb.items()[randint(0, len(pcb)-1)][0] #release random process
        except ValueError:
            print "Danger: %s %d" % (pcb, n)
            m.print_memory()
        #print "releasing memory at %d" % p
        m.mm_release(p)
        #raw_input("press enter to continue")
    if next_fit: print "next-fit: ",
    else: print "first-fit:",
    print "a: %d d: %d" % (a, d),
    print "utilization: %3.1f%% performance: %5.3f holes/iteration" %\
            ((avg_util / iter) * 100, m.get_avg_holes())
    print >>fout, "%5.3f %3.1f" % (avg_n/n_count, m.get_avg_holes())

if __name__ == "__main__":
    pcb = {}
    m = mem(pcb)
    iter = 1000
    f1 = file("first_graph_data", 'w')
    f2 = file("next_graph_data", 'w')
    for a in range(2, 20, 4):
        for d in range(4, 13, 4):
            test_mem(m, iter, a, d, pcb, 0, f1)
            test_mem(m, iter, a, d, pcb, 1, f2)
