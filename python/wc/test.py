import timeit

def isEven1(n): return not n&1
def isEven2(n): return n % 2 == 0
def isEven3(n): return n % 2 and 'Odd' or 'Even'
def isEven4(n): return n % 2 and False or True
def isEven5(n): return n % 2 and 0 or 1

def testfunc(predicate):
   for i in range(100): predicate(i)

def testtimes():
   for function in ('isEven1', 'isEven2', 'isEven3', 'isEven4', 'isEven5'):
       t = timeit.Timer('testfunc(%s)' % (function), \
           'from __main__ import %s, testfunc' % function)
       print "%s: %f" % (function, t.timeit(100000))

if __name__ == "__main__": testtimes()
