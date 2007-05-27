#Problem: arrange the numbers range(1, 13) on a six-point star so that every 
#line adds up to 26. ASCII:
#              1     2
#                 3
#              4     5
#           6           7
#              8     9
#                 10
#             11     12
#
# the vertices are 1,2,6,7,11,12. The 7 lines are 
#(1,3,5,7), (1,4,8,11), 
#(2,3,4,6), (2,5,9,12), 
#(7,9,10,11), 
#(12,10,8,6), 
#
# So, given twelve variables:
#vars = range(1, 13)
#
# we can test if we have a satisfied solution:
def solved(vars):
    #lines are the same as above, but with one subtracted from each number to
    #make them into array indices
    lines = [(0,2,4,6), (0,3,7,10), 
    (1,2,3,5), (1,4,8,11), 
    (6,8,9,10), 
    (11,9,7,5)] 
    for line in lines:
        if sum(vars[i] for i in line) != 26:
            return False
    return True

#and, as a gut check, let's figure out a way to print it
def pretty_print_star(permutation):
    print """
              %s     %s
                 %s
              %s     %s
           %s           %s
              %s     %s
                 %s
              %s     %s
""" % tuple(permutation)

# now! the brute force way to search for solutions would be to try and permute
# a list of all 12 integers, and wait for "solved" to be true. I'll import a
# permute function:
# (requires probstat: http://probstat.sourceforge.net/ )
import probstat
for perm in probstat.Permutation(range(1, 13)):
    if solved(perm):
        print "found solution!"
        pretty_print_star(perm)
