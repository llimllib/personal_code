#!/usr/bin/python

# Here's the problem: generate as many integers as possible from 0 to inf using
# 4 copies of some int n (the original problem specified 4) and only the
# operations +,-,*,/, in any parenthesization.
# I've been trying to show that (n op (n op n)) op n) is always redundant. 
# Things I know:
# a) it's not: ((n-(n*n))\n) results in 1-n, the only unique value created
#    by this parenthesization on the integers. All other operations yield a
#    result which can be reduced to some other equation

# So, what about (n op ((n op n) op n))? What's its unique value?

def test_num(num):
    integers = range(-256,256)
    results = [ [] for n in range(5)]

    maths = ['+', '-', '*', '/']

    #all possible parenthesizations
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
                    if result in integers:
                        temp.replace('num', str(num))
                        results[op].append([temp, result])
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
    """sort a list and return it"""
    list.sort(); return list

def four_unique(results):
    res = []
    three = results.pop(3)
    for set in results:
        for elt in set:
            if elt[1] not in res: res.append(elt[1])
    #print _sort(res)
    in4 = []
    for elt in three:
        if elt[1] not in in4: in4.append(elt[1])
        if elt[1] not in res: print "%s=%d" % (elt[0], elt[1])
    #print _sort(in4)

def find_num(results, num):
    for set in results:
        for elt in set:
            if num == elt[1]: print elt

if __name__=="__main__":
    for i in range(5,6):
        print "testing %d..." % i
        results = test_num(i)
        sort_results(results)
        four_unique(results)
        #find_num(results, -3)
