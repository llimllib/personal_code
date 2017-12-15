# runs in about 5s on my system, with:
#
# cythonize -b b.pyx
# python -c "import b; print(b.go(783, 325, 40000000)); print(b.go2(783, 325, 5000000)"
def go(int basea, int baseb, int iters):
    cdef int judge = 0
    for i in range(iters):
        basea = basea * 16807 % 2147483647
        baseb = baseb * 48271 % 2147483647
        if basea & 0xFFFF == baseb & 0xFFFF:
            judge += 1
    return judge

def go2(int a, int b, int iters):
    cdef int judge = 0
    for i in range(iters):
        while 1:
            a = a * 16807 % 2147483647
            if a % 4 == 0: break
        while 1:
            b = b * 48271 % 2147483647
            if b % 8 == 0: break
        if a & 0xFFFF == b & 0xFFFF:
            judge += 1
    return judge
