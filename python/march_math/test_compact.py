#!/usr/bin/python
import sys

def test_num(num):
    sz = num**num
    if sz < 100000: integers = range(num**num)
    else: integers = range(100000)
    results = [ [] for n in range(5)]

    maths = ['+', '-', '*', '/']

    ops = ["(((num %s num) %s num) %s num)", "(num %s (num %s (num %s num)))",
    "((num %s num) %s (num %s num))", "(num %s ((num %s num) %s num))",
    "((num %s (num %s num)) %s num)"]

    for f in maths:
        for g in maths:
            for h in maths:
                for op in range(len(ops)):
                    try:
                        temp = ops[op] % (f,g,h)
                        result = eval(temp)
                    except ZeroDivisionError: result = .1
                    if result in integers: results[op].append([temp, result])
    return results

def sort_results(results):
    def cmpfunc(x, y):
        if x[1] > y[1]: return 1
        else:           return -1
    for set in results: set.sort(cmpfunc)

def print_unique(results):
    already_printed = []
    count = 0
    for set in results:
        count += 1
        s = ''
        for elt in set:
            if elt[1] not in already_printed:
                s += str(elt[1]) + ' '
                already_printed.append(elt[1])
                if count > 3:
                    print "%s = %d" % (elt[0].replace("num", str(num)), elt[1])
        print s

def find_equal(results):
    for i in range(len(results)):
        print "set %d has %d elements" % (i, len(results[i]))
        if i != 4:
            eq = 1
            for j in range(len(results[i])):
                if results[i][1] != results[4][1]:
                    print "set %d is not equal to set 4" % i
                    eq = 0
                    break
            if eq: print "set %d is equal to set 4" % i

def _sort(list):
    """sort a list *and return it*"""
    list.sort(); return list

def four_unique(results):
    res = []
    four = results[4]
    del results[4]
    for set in results:
        for elt in set:
            if elt[1] not in res: res.append(elt[1])
    #print _sort(res)
    in4 = []
    for elt in four:
        if elt[1] not in in4: in4.append(elt[1])
        if elt[1] not in res:
            print "four came up with a unique!!!"
            raise "stop"
    #print _sort(in4)

def three_four(results):
    res = []
    four = results[4]
    del results[4]
    for elt in results[3]:
        if elt[1] not in res: res.append(elt[1])
    #print _sort(res)
    in4 = []
    for elt in four:
        if elt[1] not in in4: in4.append(elt[1])
        if elt[1] not in res:
            print "four came up with a unique!!!"
            raise "stop"
    #print _sort(in4)

if __name__=="__main__":
    for i in range(4,5):
        print "testing %d..." % i
        results = test_num(i)
        sort_results(results)
        three_four(results)
        #sort_results(results)
