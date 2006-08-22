from sets import Set
from combine import xcombinations
fout = ('data.out', 'w')
bin = (0,1)
ss = [[a,b,c,d,e,f] for a in bin for b in bin for c in bin for d in bin
            for e in bin for f in bin]
sigs = []
for s in ss:
    if s not in sigs and s.reverse() not in sigs: sigs.append(s)
#print len(sigs)

def diff(a, b):
    if a == b or a.reverse() == b: return True
    return False

def cols(square):
    for i in range(6):
        yield [row[i] for row in square]

def magic_s(square):
    col_sigs = []
    broken = False
    for col in cols(square):
        if col not in col_sigs and col.reverse() not in col_sigs:
            col_sigs.append(col)
        else:
            broken = True
            break
    if not broken: return square

def stacks(cube):
    for i in range(6):
        for j in range(6):
            yield [square[i][j] for square in cube]

def magic_c(cube):
    stack_sigs = []
    broken = False
    for stack in stacks(cube):
        if stack not in stack_sigs and stack.reverse() not in stack_sigs:
            stack_sigs.append(stack)
        else:
            broken = True
            break
    if not broken: return cube

def print_cube(cube):
    for square in cube:
        for row in square:
            for elt in row:
                print elt,
            print ' '
        print

#squares = []
#tried = Set()
#for square in xcombinations(sigs, 6):
#    if magic_s(square):
#        squares.append(square)
#    if len(squares) > 10000: break
#print "generated 10000 squares"
##the problem here is that there is a large number of magic squares.
#i = 0
#for cube in xcombinations(squares, 6):
#    i += 1
#    if not i % 100000: print "cube combination %d" % i
#    if magic_c(cube):
#        print_cube(cube)
#        break
