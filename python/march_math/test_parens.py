#!/usr/bin/python
num = 5.0
integers = range(1000)
results = [ [] for n in range(5)]

maths = ['+', '-', '*', '/']

ops = ["(((num %s num) %s num) %s num)", "(num %s (num %s (num %s num)))",
"((num %s num) %s (num %s num))", "(num %s ((num %s num) %s num))",
"((num %s (num %s num)) %s num)"]

for f in maths:
    for g in maths:
        for h in maths:
            #(((4+4)+4)+4)
            op = "(((num %s num) %s num) %s num)" % (f, g, h)
            result = eval(op)
            if result in integers: results[0].append([op, result])
            #(4+(4+(4+4)))
            try: 
                op = "(num %s (num %s (num %s num)))" % (f, g, h)
                result = eval(op)
            except ZeroDivisionError: result = .1
            if result in integers: results[1].append([op, result])
            #((4+4)+(4+4))
            try:
                op = "((num %s num) %s (num %s num))" % (f, g, h)
                result = eval(op)
            except ZeroDivisionError: result = .1
            if result in integers: results[2].append([op, result])
            #(4+((4+4)+4))
            try:
                op = "(num %s ((num %s num) %s num))" % (f, g, h)
                result = eval(op)
            except ZeroDivisionError: result = .1
            if result in integers: results[3].append([op, result])
            #((4+(4+4))+4) - I have never found a new elt with this set
            # always produces 1 less than the previous set
            # produces numbers that the previous set does not
            try:
                op = "((num %s (num %s num)) %s num)" % (f, g, h)
                result = eval(op)
            except ZeroDivisionError: result = .1
            if result in integers: results[4].append([op, result])


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

def three_four(results):
    for i in range(len(results[4])):
        if results[3][i] != results[4][i]: print results[3][i], results[4][i]

if __name__=="__main__":
    sort_results(results)
    find_equal(results)
