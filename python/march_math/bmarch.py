from math import sqrt

num = 4.0
integers = range(101)
results = []

#we need names for the functions so we can iterate through them
def div(a, b): return a / b
def mult(a, b): return a * b
def plus(a, b): return a + b
def minus(a, b): return a - b
maths = [div, mult, plus, minus]

def op(func):
    """this function returns a symbol for the given function"""
    if func.__name__ == "div": return "/"
    if func.__name__ == "mult": return "*"
    if func.__name__ == "plus": return "+"
    if func.__name__ == "minus": return "-"

for f in maths:
    for g in maths:
        for h in maths:
            result = f(g(h(num, num), num), num) #can't have a zero divisor
            if result in integers and result not in results:
                print "(((4 %s 4) %s 4) %s 4) = %s" % \
                    (op(f), op(g), op(h), result)
                results.append(result)
            try: 
                result = f(num, g(num, h(num, num)))
            except ZeroDivisionError: 
                result = .1
            if result in integers and result not in results:
                print "(4 %s (4 %s (4 %s 4))) = %s" % \
                    (op(f), op(g), op(h), result)
                results.append(result)
            try: 
                result = f(g(num, num), h(num, num))
            except ZeroDivisionError: 
                result = .1
            if result in integers and result not in results:
                print "((4 %s 4) %s (4 %s 4)) = %s" % \
                    (op(f), op(g), op(h), result)
                results.append(result)
#f(n, g(h(n, n), n))
#f(g(n, h(n, n)), n)
#are these new parenthesizations? do they yield anything new? no.
results.sort()
print results
