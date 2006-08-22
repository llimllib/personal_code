#!/usr/bin/python
num = 4.0
integers = range(101)
results = [ [] for n in range(5)]

maths = ['+', '-', '*', '/']

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
            #((4+(4+4))+4)
            try:
                op = "((num %s (num %s num)) %s num)" % (f, g, h)
                result = eval(op)
            except ZeroDivisionError: result = .1
            if result in integers: results[4].append([op, result])

def cmpfunc(x, y):
    if x[1] > y[1]: return 1
    else:           return -1
results.sort(cmpfunc)

already_printed = []
for r in results: 
    if r[1] not in already_printed:
        r[0] = r[0].replace('num', str(num))
        print r[0], ' ', r[1]
        already_printed.append(r[1])
